library(shiny)


shinyServer(function(input, output) {
      
  #
  # create samples
  #
  # creates t = input$trials, each of size n = input$size samples
  # from the parameters specified by the user 
  # input$mean is the specified mean 
  # note: new trials are NOT generated when a new coverage rate is set
  #
  # output is a t length vector, where each 
  # element is 2 times the sum of the sample 
  #
  trials = reactive({ 
    a = input$action # causes refresh of all plots
    sapply( 1:input$trials, function(k) {
       sample = rexp(input$size, 1/input$mean) 
       2 * sum(sample)
    }) 
  })
  
  #
  # convert the randomly generated sums into to 
  # confidence intervals for means (for plotting)
  #
  #
  # functions for confidence interal for mean
  #   
  #   this function searches for shortest CI for given coverage rate and df
  # 
  grid.min.chisq = function(cr, df) {
    inc = 0.0001 # increment for search 
    bpinit = (1 + cr) / 2 # begin at (1 + cr) / 2
    m = (1 - bpinit)/inc # search until left point is zero
    
    # determine length of interval at each increment
    len = sapply(0:(m-1), function(i) {
      bp = bpinit - inc*i
      ap = bp - cr
      qchisq(bp, df) - qchisq(ap, df)
    })
    
    # pick out index of minium interval
    n = order(len)[1] - 1 
    bp = bpinit - inc * n
    ap = bp - cr
    # return quantiles that give minimum interval on points searched
    c(qchisq(ap, df), qchisq(bp, df))  
  }

  #
  # make intervals for mean
  #
  # output is t by 8
  #
  make.interval.mean = function(trials, cr, n, m) {
           
    qab = grid.min.chisq(cr, 2*n) 
    lower = trials / qab[2] 
    upper = trials / qab[1] 
    cover = 2L - as.integer(lower <= m & m <= upper)
    
    length = upper - lower
    expectlen = 2 * n * m * (1/qab[1] - 1/qab[2])
    varlen = 4 * n * (m^2) * (1/qab[1] - 1/qab[2])^2
    data.frame(lower, upper, length, cover, trials, qab[1], qab[2], expectlen)
  }
  
  interval.mean = reactive({ make.interval.mean( trials(), 
    input$cr, input$size, input$mean ) })
  
  #
  # creates 2 plots, the first is the confidence intervals 
  # the second is a summary of their lengths
  #
  make.plots = function( interval ) {
    #
    # blue for intervals containing parameter (mean or variance), 
    # red for intervals that do not
    infocol = c("blue", "red") 
    cr = input$cr
    t = input$trials
    n = input$size
    m = input$mean
    
    par(mfrow=c(1,2))
    # draw empty plot area
    notcover = t - sum(interval[, 4] - 1)
    subtitle1 = paste0(notcover, " (",round(notcover/t,3),") CI include the")
    subtitle2 = paste("mean of", m) 
    plot(0,0, type = "n", 
         xlim = c(0, m*3),  # right boundary is 1.2 times sd (sd = m^2)
         xlab = paste(subtitle1, subtitle2), 
         ylim = c(t,1), # this reverses ordering on y-axis so matches table 
         ylab = "Trial",
         main = "Confidence Intervals for Mean")
    mtext( paste("Coverage",cr,"  ",t, "trials; each trial has",n,"samples") )
    # plot the trials, going from 1 at top to t at bottom
    for (k in 1:t) lines( interval[k, c(1:2) ], c(k, k),
                           col = infocol[ interval[k, 4] ] )
    abline(v = m, col = "green")
    
    #
    # 2nd plot: lengths of intervals
    #
    plot(density(interval[,3]), main="Interval Lengths",
      # xlim = xrangeLen, 
      xlab = "interval length"
      # ylim = yrangeLen, 
    )
    abline(v = interval[1, "expectlen"], col = "green")
    mtext("Expected length in green")
  }
  
  #
  # create plots and display data for each tab
  #
  output$mean.unknown.Plot <- renderPlot( make.plots( interval.mean() ) )
  output$interval = renderTable({ interval.mean() })
})
