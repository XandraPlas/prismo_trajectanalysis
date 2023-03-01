plot_growth_mixture <- function(x,
                                items = NULL,
                                bw = FALSE,
                                rawdata = FALSE,
                                estimated = TRUE,
                                poly = FALSE,
                                alpha_range = c(0, 0.1),
                                growth_variables = NULL,
                                time_scale = NULL,
                                jitter_lines = NULL,
                                coefficients = "unstandardized")
{
  submods <- names(x@submodels)
  if (is.null(growth_variables)) {
    growth_variables <- x[[submods[1]]]$latentVars
  }
  if (is.null(items)) {
    items <- x[[submods[1]]]$manifestVars
  }
  loadings <- lapply(submods, function(thismod) {
    x[[thismod]]$A$values[items, growth_variables]
  })
  
  estimates <- lapply(submods, function(thismod) {
    matrix(
      x[[thismod]]$M$values[1, growth_variables],
      nrow = nrow(loadings[[1]]),
      ncol = length(growth_variables),
      byrow = TRUE
    )
  })
  
  if (is.null(time_scale)) {
    time_scale <- seq_along(items)
  }
  
  predicted_trajectories <- do.call(rbind,
                                    lapply(1:length(submods), function(x) {
                                      data.frame(
                                        Time = time_scale,
                                        Value = rowSums(loadings[[x]] * estimates[[x]]),
                                        Class = submods[x]
                                      )
                                    }))
  predicted_trajectories$Class <-
    ordered(predicted_trajectories$Class, levels = submods)
  line_plot <- ggplot(NULL)
  
  if (rawdata) {
    rawdata <- x$data$observed[, items]
    cprobs <-
      class_prob(x, type = "individual")[["individual"]][, submods, drop = FALSE]
    rawdata <- cbind(rawdata, cprobs)
    names(rawdata)[match(submods, names(rawdata))] <-
      paste0("Probability.", seq_along(submods))
    rawdata <-
      reshape(
        rawdata,
        direction = "long",
        varying = paste0("Probability.", seq_along(submods)),
        timevar = "Class",
        idvar = "ID"
      )
    names(rawdata)[match(items, names(rawdata))] <-
      paste0("Value.", items)
    rawdata <- reshape(
      rawdata,
      direction = "long",
      varying = paste0("Value.", items),
      timevar = "Time"
    )[, c("ID", "Time", "Value", "Class", "Probability")]
    rawdata$Time <- ordered(rawdata$Time, levels = items)
    rawdata$Time <- time_scale[as.numeric(rawdata$Time)]
    rawdata$Class <- ordered(rawdata$Class, labels = submods)
    rawdata$ID <- paste(rawdata$Title, rawdata$Class,
                        rawdata$ID, sep = "")
    if (!is.null(jitter_lines)) {
      rawdata$Value <- rawdata$Value + stats::rnorm(nrow(rawdata),
                                                    sd = (jitter_lines * stats::sd(rawdata$Value,
                                                                                   na.rm = TRUE)))
    }
    if (bw) {
      line_plot <- line_plot + geom_path(data = rawdata,
                                         aes(
                                           x = Time,
                                           y = Value,
                                           group = ID,
                                           linetype = Class,
                                           alpha = Probability
                                         )) +
        scale_alpha_continuous(range = alpha_range,
                               guide = "none")
    }
    else {
      line_plot <- line_plot + geom_path(
        data = rawdata,
        aes(
          x = Time,
          y = Value,
          group = ID,
          linetype = Class,
          colour = Class,
          alpha = Probability
        )
      ) +
        scale_alpha_continuous(range = alpha_range, guide = "none")
    }
  }
    if (estimated) {
      if (bw) {
        line_plot <- line_plot + geom_point(
          data = predicted_trajectories,
          aes(
            x = Time,
            y = Value,
            group = Class,
            shape = Class
          ),
          size = 2
        ) + geom_line(
          data = predicted_trajectories,
          aes(
            x = Time,
            y = Value,
            group = Class,
            linetype = Class
          ),
          linewidth = 1
        )
      }
      else {
        line_plot <- line_plot + geom_point(
          data = predicted_trajectories,
          aes(
            x = Time,
            y = Value,
            group = Class,
            shape = Class,
            colour = Class
          ),
          size = 2
        ) +
          geom_line(
            data = predicted_trajectories,
            aes(
              x = Time,
              y = Value,
              group = Class,
              linetype = Class,
              colour = Class
            ),
            linewidth = 1
          )
      }
    }
    line_plot <-
      line_plot + theme_bw() + scale_x_continuous(expand = c(0,
                                                             0),
                                                  breaks = time_scale,
                                                  labels = time_scale) + scale_y_continuous(expand = c(0,
                                                                                                       0))
    return(line_plot)
  }
  