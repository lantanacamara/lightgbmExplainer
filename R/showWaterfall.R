#' Step 3: Get prediction breakdown and waterfall chart for a single row of data
#'
#' This function prints the feature impact breakdown for a single data row, and plots an accompanying waterfall chart.
#' @param lgb.model A trained lightgbm model
#' @param explainer The output from the buildExplainer function, for this model
#' @param lgb.dtrain The lgb.dtrain in which the row to be predicted is stored
#' @param lgb.train.data The matrix of data from which the lgb.dtrain was built
#' @param idx The row number of the data to be explained
#' @param type The objective function of the model - either "binary" (for binary:logistic) or "regression" (for reg:linear)
#' @param threshold Default = 0.0001. The waterfall chart will group all variables with absolute impact less than the threshold into a variable called 'Other'
#' @return None
#' @export
#' @import data.table
#' @import lightgbm
#' @import waterfalls
#' @import scales
#' @import ggplot2

showWaterfall = function(lgb.model, explainer, lgb.dtrain, lgb.train.data, id, type = "binary", threshold = 0.0001){


  breakdown = explainPredictions(lgb.model, explainer, lgb.train.data[id,,drop=FALSE])

  weight = rowSums(breakdown)
  if (type == 'regression'){
    pred = weight
  }else{
    pred = 1/(1+exp(-weight))
  }


  breakdown_summary = as.matrix(breakdown)[1,]
  data_for_label = lgb.train.data[id,]

  idx = order(abs(breakdown_summary),decreasing=TRUE)
  breakdown_summary = breakdown_summary[idx]
  data_for_label = data_for_label[idx]

  intercept = breakdown_summary[names(breakdown_summary)=='intercept']
  data_for_label = data_for_label[names(breakdown_summary)!='intercept']
  breakdown_summary = breakdown_summary[names(breakdown_summary)!='intercept']

  idx_other =which(abs(breakdown_summary)<threshold)
  other_impact = 0

  if (length(idx_other > 0)){
    other_impact = sum(breakdown_summary[idx_other])
    names(other_impact) = 'other'
    breakdown_summary = breakdown_summary[-idx_other]
    data_for_label = data_for_label[-idx_other]
  }

  if (abs(other_impact) > 0){
    breakdown_summary = c(intercept, breakdown_summary, other_impact)
    data_for_label = c("", data_for_label,"")
    labels = paste0(names(breakdown_summary)," = ", data_for_label)
    labels[1] = 'intercept'
    labels[length(labels)] = 'other'
  }else{
    breakdown_summary = c(intercept, breakdown_summary)
    data_for_label = c("", data_for_label)
    labels = paste0(names(breakdown_summary)," = ", data_for_label)
    labels[1] = 'intercept'
  }


  if (!is.null(lgb.dtrain)){
    if (!is.null(lgb.dtrain$getinfo("label")[id])){
      cat("\nActual: ", lgb.dtrain$getinfo("label")[id])
    }
  }
  cat("\nPrediction: ", pred)
  cat("\nWeight: ", weight)
  cat("\nBreakdown")
  cat('\n')
  print(breakdown_summary)

  if (type == 'regression'){

  waterfall(values = round(breakdown_summary,2), labels = labels
            , calc_total = TRUE
            , total_axis_text = "Prediction")  + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }else{

    inverse_logit_trans <- trans_new("inverse logit",
                                     transform = plogis,
                                     inverse = qlogis)

    inverse_logit_labels = function(x){return (1/(1+exp(-x)))}
    logit = function(x){return(log(x/(1-x)))}

    ybreaks<-logit(seq(2,98,2)/100)

    waterfall(values = round(breakdown_summary,2), labels = labels
              , calc_total = TRUE
              , total_axis_text = "Prediction")  + scale_y_continuous(labels = inverse_logit_labels, breaks = ybreaks) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

  }
}
