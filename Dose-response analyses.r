rm(list = ls())
library(ggplot2)
library(dplyr)
library(patchwork)
library(rms)

## required packages : ggplot2 | dplyr | patchwork | rms ##

# set workplace
setwd("D:/stata16/dta/metabolic_accelerometer_physical/Sample")

# read all data
baseline <- haven::read_dta("all_mortality_RCS.dta")

X <- c("light_PA_min", "moderate_PA_min", "vigorous_PA_min", "MVPA_min")
Y <- c("death_HES", "death_cancer", "death_CVD")
covs <- c("age_accel", "sex", "new_ethnic", "qualification","season_wear", 
          "duration_wear", "smoke", "alcohol_unit", "diet_score", "sleep_score1",
          "diabetes_duration", "BMI", "wasit", "history_cancer_CVD", "self_hypertension", 
          "health_self", "long_illness_injury", "illness_injury_2years")

xtitle <- c("LPA (minutes/week)","MPA (minutes/week)","VPA (minutes/week)","MVPA (minutes/week)")
ytitle <- c("all-cause mortality","cancer mortality","cardiovascular disease mortality")

# loop part
for(j in 1:length(Y)){
  for(i in 1:length(X)){
    x <- X[i]
    y <- Y[j]
    xt <- xtitle[i]
    yt <- ytitle[j]
    kn <- 4
    
    indf <- dplyr::select(baseline, all_of("follow_death"), all_of(covs), all_of(y), all_of(x)) %>%
      dplyr::rename("y"=y,"x"=x,"time"="follow_death")
    
    indf <- as.data.frame(lapply(indf, as.numeric))
    
    lim1 <- quantile(indf$x,probs = c(0.01, 0.99))[1]
    lim99 <- quantile(indf$x,probs = c(0.01, 0.99))[2]
    
    dd <- NULL
    dd <<- rms::datadist(indf)
    options(datadist='dd')
    formula <- paste0("survival::Surv(time,y) ~ rms::rcs(x,", kn, ")", " + ", paste0(covs, collapse = " + "))
    model <- rms::cph(as.formula(formula), data = indf, x = T, y = T, surv = T)
    pvalue_all <- anova(model)[1,3]
    pvalue_nonlin <- round(anova(model)[2,3], 3)
    pre.model <- rms::Predict(model, x, fun = exp, 
                              type = "predictions", ref.zero = T, conf.int = 0.95, digits = 2)
    
    refvalue <- lim1
    dd <- rms::datadist(indf)
    dd[["limits"]]["Adjust to", "x"] <- refvalue
    old <- options()
    on.exit(options(old))
    options(datadist = "dd")
    model <- update(model)
    model.logistic <- model
    pre.model <- rms::Predict(model.logistic, x, fun = exp, 
                              type = "predictions", ref.zero = T, conf.int = 0.95, digits = 2)
    
    df <- as.data.frame(dplyr::select(pre.model, x, yhat, lower, upper))
    
    assign(paste0("df",i,"_",j),df)
    
    colnames(df) <- c("x", "y", "lower", "upper")
    
    label1 <- paste0("P[Overall]", ifelse(pvalue_all < 0.001, 
                                          "<0.001", paste0("==",sprintf("%.3f", pvalue_all))))
    label2 <- paste0("P[Nonlinear]",ifelse(pvalue_nonlin < 0.001, 
                                           "<0.001", paste0("==",sprintf("%.3f", pvalue_nonlin))))
    
    yt <- paste0("HR(95% CI) for ",yt)
    
    p <- ggplot() +
      geom_ribbon(data = df, aes(x = x, ymin = lower, ymax = upper),
                  fill = "#ccdfff", alpha = 0.9) +
      geom_line(data = df, 
                aes(x = x, y = y), color = "#2f7eff", size = 1) +
      scale_x_continuous(xt, limits = c(lim1-5,lim99)) +
      scale_y_continuous(yt, limits = c(0,1), breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                         oob = scales::rescale_none) +
      geom_hline(yintercept = 1, linetype = 2, color = "black", size = 0.75) +
      theme_minimal() + 
      
      annotate("text",x=Inf,y=Inf,hjust=1.2,vjust=3.4,label=label1,
               parse=T,color="black",size=4) +
      annotate("text",x=Inf,y=Inf,hjust=1.2,vjust=4.4,label=label2,
               parse=T,color="black",size=4)
    if(i==1){
      p <- p + theme(panel.grid = element_blank(), 
                     panel.border = element_rect(fill = NA,size=1,color="black"),
                     axis.title.x = element_blank(),
                     axis.title.y = element_text(size=12,color="black",vjust=7),
                     axis.text.y = element_text(size=12,color="black",hjust=0.5,vjust=4,angle=90),
                     axis.text.x = element_text(size=12,color="black",vjust=-1.5),
                     axis.ticks = element_line(size=0.8),
                     plot.margin=unit(rep(2,4),'lines'),
                     plot.title = element_text(size=10))
    } else {
      p <- p + theme(panel.grid = element_blank(), 
                     panel.border = element_rect(fill = NA,size=1,color="black"),
                     axis.title = element_blank(),
                     axis.text.y = element_text(size=12,color="black",hjust=0.5,vjust=4,angle=90),
                     axis.text.x = element_text(size=12,color="black",vjust=-1.5),
                     axis.ticks = element_line(size=0.8),
                     plot.margin=unit(rep(2,4),'lines'),
                     plot.title = element_text(size=10))
    }
    if(i==2){
      p <- p + geom_vline(xintercept = 150, linetype = 3, color = "black", size = 0.75) 
        
    } else if (i==3) {
      p <- p + geom_vline(xintercept = 75, linetype = 3, color = "black", size = 0.75) 
        
    } else if (i==4){
      p <- p + 
        scale_x_continuous(xt, limits = c(lim1-5,lim99), breaks = c(0,300,600,900,1200))
    }
    
    breaks <- seq(min(indf[, "x"]), max(indf[, "x"]), length = 40)
    if(x=="vigorous_PA_min"){breaks <- seq(min(indf[, "x"]), max(indf[, "x"]), length = 80)}
    
    h <- hist(indf[, "x"], breaks = breaks, plot = F)
    dh <- data.frame(x = h[["mids"]], freq = h[["counts"]])
    h <- ggplot(dh) + 
      geom_bar(aes(x = x, y = freq), stat = "identity", fill = "lightgrey", color = "black", size = 0.6) +
      scale_x_continuous(xt, limits = c(lim1-5,lim99),oob = scales::rescale_none) + 
      ggtitle(xt) +
      theme_minimal()
    
    if(i==1){
      h <- h + theme(panel.grid = element_blank(),
                     axis.title.x = element_blank(),
                     axis.title.y = element_text(size=12,color="black",vjust=7),
                     axis.text.y = element_text(size=12,color="black",hjust=0.5,vjust=4,angle=90),
                     axis.text.x = element_text(size=12,color="black",vjust=-1.5),
                     axis.ticks = element_line(size=0.8),
                     axis.line = element_line(size=0.8),
                     plot.margin=unit(rep(2,4),'lines'),
                     plot.title = element_text(size=16, hjust=0.5,face = "bold"),
      ) +
        scale_y_continuous("Frequency", breaks = c(0,floor(max(dh$freq)/100)*100), expand = c(0,0),
                           oob = scales::rescale_none)
    } else {
      h <- h + theme(panel.grid = element_blank(),
                     axis.title = element_blank(),
                     axis.text.y = element_text(size=12,color="black",hjust=0.5,vjust=4,angle=90),
                     axis.text.x = element_text(size=12,color="black",vjust=-1.5),
                     axis.ticks = element_line(size=0.8),
                     axis.line = element_line(size=0.8),
                     plot.margin=unit(rep(2,4),'lines'),
                     plot.title = element_text(size=16, hjust=0.5,face = "bold"),
      ) +
        scale_y_continuous(breaks = c(0,floor(max(dh$freq)/100)*100), expand = c(0,0),
                           oob = scales::rescale_none)
    }
    
    assign(paste0("h",i), h)
    assign(paste0("p",i,"_",j), p)
    
  }
}
