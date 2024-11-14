#' Non-parametric link analysis between two samples
#'
#' Check for any kind of linkage between two unrelated samples
#' @param sample1 The first sample
#' @param sample2 The second sample
#' @param type1 the data type of first sample (can be interval, ordinal and nominal)
#' @param type2 the data type of second sample (can be interval, ordinal and nominal)
#' @return The score of a linkage (mostly p-value, can be also a z-score in Mann-Whitney Test, or correlation coefficient in Spearman Correlation Test)
#' @examples
#' result <- nonparam_link_analysis(c(1, 2, 3, 4, 5), c(6, 7, 8, 9, 10), "int", "ord")
#' result <- nonparam_link_analysis(c("A", "A", "B", "C"), c(58, 70, 64, 19), "ord", "nom")
#' @export

nonparam_link_analysis <- function(sample1, sample2, type1, type2)
{
  valid_types <- c("int", "ord", "nom")
  if (!is.vector(sample1) || !is.vector(sample2))
  {
    stop("Error: At least one of the samples is not a vector.")
    result <- 1
  }
  else if (length(sample1) !=  length(sample2))
  {
    stop("Error: Different samples length. Ensure your data samples have the same lengths.")
    result <- 1
  }
  else if (any(is.na(sample1)) || any(is.na(sample2)))
  {
    stop("Error: Samples should not contain NA values.")
    result <- 1
  }
  else if (!(type1 %in% valid_types) || !(type2 %in% valid_types))
  {
    stop("Error: At least one of the types is not entered correctly")
    result <- 1
  }
  else
  {
    if (type1 == "nom")
    {
      if (type2 == "nom")
      {
        df <- data.frame(
          V1 = sample1,
          V2 = sample2
        )
        table_to_test <- table(df$V1, df$V2)

        if (any(as.vector(table_to_test) < 5)) {
          result <- fisher.test(table_to_test)
        } else {
          result <- chisq.test(table_to_test)
        }
      }
      else if (type2 == "ord")
      {
        df <- data.frame(
          V1 = as.numeric(factor(sample1)),
          V2 = factor(sample2)
        )

        result <- kruskal.test(df$V1, df$V2)
      }
      else if (type2 == "int")
      {
        df <- data.frame(
          V1 = as.numeric(factor(sample1)),
          V2 = sample2
        )

        result <- kruskal.test(df$V1, df$V2)
      }
      else
      {
        stop("Error: No such data type. Valid values are \"nom\", \"ord\", \"int\" ")
        result <- 1
      }
    }
    else if(type1 == "ord")
    {
      if (type2 == "nom")
      {
        df <- data.frame(
          V1 = as.numeric(factor(sample1)),
          V2 = as.factor(sample2)
        )

        result <- kruskal.test(df$V1, df$V2)
      }
      else if (type2 == "ord")
      {
        df <- data.frame(
          V1 = as.numeric(factor(sample1, levels = unique(sample1))),
          V2 = as.numeric(factor(sample2, levels = unique(sample2))))

        result <- cor.test(df$V1, df$V2, method = "pearson")
      }
      else if (type2 == "int")
      {
        df <- data.frame(
          V1 = as.numeric(factor(sample1)),
          V2 = factor(sample2)
        )

        result <- kruskal.test(df$V1, df$V2)
      }
      else
      {
        stop("Error: No such data type. Valid values are \"nom\", \"ord\", \"int\" ")
        result <- 1
      }
    }
    else if(type1 == "int")
    {
      if (type2 == "nom")
      {
        df <- data.frame(
          V1 = sample1,
          V2 = sample2
        )

        table_to_test <- table(df$V1, df$V2)

        if (any(as.vector(table_to_test) < 5)) {
          result <- fisher.test(table_to_test)
        } else {
          result <- chisq.test(table_to_test)
        }
      }
      else if (type2 == "ord")
      {
        df <- data.frame(
          V1 = sample1,
          V2 = factor(sample2))

        result <- kruskal.test(df$V1, df$V2)
      }
      else if (type2 == "int")
      {
        ## check for normality

        df <- data.frame(
          V1 = sample1,
          V2 = sample2)

        result <- cor.test(df$V1, df$V2, method = "pearson")
      }
      else
      {
        stop("Error: No such data type. Valid values are \"nom\", \"ord\", \"int\" ")
        result <- 1
      }
    }
    else
    {
      stop("Error: No such data type. Valid values are \"nom\", \"ord\", \"int\" ")
      result <- 1
    }
  }
  return(result)
}
