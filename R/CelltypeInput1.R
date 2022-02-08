
#' Load a Matrix
#'
#' This function loads a file as a matrix. It assumes that the first column
#' contains the rownames and the subsequent columns are the sample identifiers.
#' Any rows with duplicated row names will be dropped with the first one being
#' kepted.
#'
#' @param infile Path to the input file
#' @return A matrix of the infile
#' @export
Pvaluecaculate = function(pvalue_test_data){
  SampleNumber<-ncol(pvalue_test_data)/2
  if(SampleNumber==3){
    for (n in 1:nrow(pvalue_test_data)){
      x1=c(pvalue_test_data[n,1],pvalue_test_data[n,2],pvalue_test_data[n,3])
      y1=c(pvalue_test_data[n,4],pvalue_test_data[n,5],pvalue_test_data[n,6])
      q1<-t.test(x1,y1, var.equal = TRUE)
      pvalue_test_data[n,'pvalue']=q1[[3]]
      
    }
  }
  if(SampleNumber==4){
    
    for (n in 1:nrow(pvalue_test_data)){
      x1=c(pvalue_test_data[n,1],pvalue_test_data[n,2],pvalue_test_data[n,3],pvalue_test_data[n,4])
      y1=c(pvalue_test_data[n,5],pvalue_test_data[n,6],pvalue_test_data[n,7],pvalue_test_data[n,8])
      q1<-t.test(x1,y1, var.equal = TRUE)
      pvalue_test_data[n,'pvalue']=q1[[3]]
      
    }
  }
  return(pvalue_test_data$pvalue)
} # function for get pvalue

#' @export
#########FC change function note this FC should be 2^(substraction)
FCcaculate = function(pvalue_test_data){
  SampleNumber<-ncol(pvalue_test_data)/2
  if(SampleNumber==3){
    
    
    for (n in 1:nrow(pvalue_test_data)){
      
      Seddata=c(pvalue_test_data[n,1],pvalue_test_data[n,2],pvalue_test_data[n,3])
      Rundata=c(pvalue_test_data[n,4],pvalue_test_data[n,5],pvalue_test_data[n,6])
      
      pvalue_test_data[n,'FC']=2^(median(Rundata)-median(Seddata))
      
    }
  }
  if(SampleNumber==4){
    
    for (n in 1:nrow(pvalue_test_data)){
      Seddata=c(pvalue_test_data[n,1],pvalue_test_data[n,2],pvalue_test_data[n,3],pvalue_test_data[n,4])
      Rundata=c(pvalue_test_data[n,5],pvalue_test_data[n,6],pvalue_test_data[n,7],pvalue_test_data[n,8])
      
      pvalue_test_data[n,'FC']=2^(median(Rundata)-median(Seddata))
      
      
    }
  }
  return(pvalue_test_data$FC)
} # function for get FC note this FC should be 2^(substraction)