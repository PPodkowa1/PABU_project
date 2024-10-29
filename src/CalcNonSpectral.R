## ---------------------------
##
## Purpose of script: Check if given USML stimulations fall under definition of 'non-spectral'
##
## Author: Dr. Benedict Hogan, Princeton University 
##
## Date Created: 2019
##
## ---------------------------
##
## Notes: This script works on a dataframe containing columns indicating the stimulation of the rel_VS, rel_SWS, rel_MWS, and rel_LWS
##   cone types. Usually these will come from pavo calulations. As in './BuntingNonSpectral.R' you may need to rename columns to conform
##	 to this function. Definitions for non-spectral from Stoddard et al 2020 (https://www.pnas.org/doi/full/10.1073/pnas.1919377117)
## 
## ---------------------------

CalcNonSpectral<-function(Summ) {
  # We'll explicitly code the different non-spectral colors
  # this also has the advantage of allowing us to easily label the colors

  purple<-c("SWS", "LWS")
  uv_red<-c("VS", "LWS")
  uv_green<-c("VS", "MWS")
  
  uv_purple<-c("VS", "SWS", "LWS")
  uv_yellow<-c("VS", "MWS", "LWS")

  ConeCatch<-data.frame(as.numeric(Summ$rel_VS), as.numeric(Summ$rel_SWS), as.numeric(Summ$rel_MWS), as.numeric(Summ$rel_LWS))
  names(ConeCatch)<-c("VS", "SWS", "MWS", "LWS")
  
  ColRanks<-apply(ConeCatch[,1:4], 1, function(x) 4-rank(x)) # rank=/=order, this gives each cone an order 0-3 (max-min)
  
  
  for(i in 1:dim(ConeCatch)[1]) { # loop and find which are these non-spectral colors
    
    ## Get current color
    col<-ColRanks[,i]
    
    ## And cone catch
    catch<-ConeCatch[i,1:4]
    
    ## Omit from ranking any colors under threshold
    thresh<-0.01
    if(any(catch<thresh)){
      print('Non-spectral analysis: relative catch under 1% detected and excluded from calculation of top 2 or top 3 cone catches')
      col[catch<thresh]<-Inf
    }
    
    ## get top two and top three stimulated cones
    toptwo<-rownames(ColRanks)[col<=1]
    topthr<-rownames(ColRanks)[col<=2]
    
    ## Secondary non-spectrals
    ConeCatch$purple[i]<-sum(is.element(toptwo,purple))==2
    ConeCatch$uv_red[i]<-sum(is.element(toptwo,uv_red))==2
    ConeCatch$uv_green[i]<-sum(is.element(toptwo,uv_green))==2
    
    ## Ternary non-spectrals
    ConeCatch$uv_purple[i]<-sum(is.element(topthr,uv_purple))==3
    ConeCatch$uv_yellow[i]<-sum(is.element(topthr,uv_yellow))==3
    
    ## Summaries
    end<-dim(ConeCatch)[2]
    ConeCatch$Secondary[i]<-sum(as.numeric(c(ConeCatch$purple[i], ConeCatch$uv_red[i], ConeCatch$uv_green[i])))>0
    ConeCatch$Tertiary[i]<-sum(as.numeric(c(ConeCatch$uv_purple[i], ConeCatch$uv_yellow[i])))>0
    ConeCatch$NonSpectral[i]<-sum(as.numeric(c(ConeCatch$Secondary[i], ConeCatch$Tertiary[i])))>0
    
  }
  
  ConeCatch<-ConeCatch[,5:dim(ConeCatch)[2]]
  
  Summ<-cbind(Summ, ConeCatch)  
  return(Summ)
  
}