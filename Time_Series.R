## Time Series
## Example 1

# Create the  data frame
prod <- data.frame(
  Year = c(2007:2014),
  Production = c(56,55,51,47,42,38,35,32)
)

# Plot of production over time
plot(prod$Year, prod$Production)

## Trend equation using sum of squares
trend <- lm(Production ~ Year,
            data = prod)
summary(trend)


# Alternatively
prod <- data.frame(
  Year = c(0:7),
  Production = c(56,55,51,47,42,38,35,32)
)


#######################################################################






We explore the data for the treatment and control through a boxplot. Based on the boxplots of blood pressure stratified by drug formulation and dosage shown below, clear differences in the distributional characteristics of the treatments were observed. For both soluble and non-soluble formulations, the boxplots indicate moderate spread in blood pressure values, with the interquartile ranges (IQRs) capturing most of the observed variability within each dosage level. The soluble treatments exhibited relatively lower median blood pressure values compared with the non-soluble treatments at corresponding dosages, suggesting a stronger blood-pressure--lowering effect. In contrast, the non-soluble formulations showed higher medians and slightly wider IQRs, particularly at the higher dosage level, indicating greater variability in response.

The control group displayed the widest overall spread among the treatments, with a higher median blood pressure and longer whiskers, reflecting increased variability in the absence of treatment. Across treatments, the distributions appeared approximately symmetric, although mild right-skewness was evident for some non-soluble dosage groups, as indicated by longer upper whiskers.

``` sas
/* Boxplot */
  proc sgplot data=bpdata_block;
vbox BloodPressure / category=drug group=dosage;
yaxis grid;
run;
```

![Boxplot for treatments and control](fact_blk2.png)

\newpage

Next, we take a look at the interaction plot. In particular, we are interested to see if there is an interaction between drug type and dosage. From the plot, we see that there is an interaction between drug and dosage. This is shown by the red and blue line. We need to confirm from the anova table.

``` sas
/* Interaction Plot */
  data bp_plot;
set bpdata_block;
if drug ^= "contrl";
run;

/* Compute mean BloodPressure for Drug × Dosage */
  proc means data=bp_plot noprint;
class drug dosage;
var BloodPressure;
output out=means_plot mean=MeanBP;
run;

/* Remove _TYPE_ and _FREQ_ rows from PROC MEANS */
  data means_plot_clean;
set means_plot;
if _TYPE_ = 3; /* Keeps only Drug × Dosage combos */
  run;

data means_plot_clean;
set means_plot;
if _TYPE_ = 3; /* Keeps only Drug × Dosage combos */
  run;

/* Get Control mean */
  proc means data=bpdata_block noprint;
where drug = "contrl";
var BloodPressure;
output out=control_mean mean=ControlMean;
run;

/* Add ControlMean to all rows for plotting */
  data plot_ready;
if _n_ = 1 then set control_mean;
set means_plot_clean;
run;

/* Interaction Plot with Control Mean Line */
  proc sgplot data=plot_ready;
series x=drug y=MeanBP / group=dosage lineattrs=(thickness=2) markers;
refline ControlMean / 
  axis=y lineattrs=(color=darkgreen pattern=shortdash thickness=2) 
label="Control Mean";
yaxis label="Average Blood Pressure";
xaxis label="drug";
keylegend / location=inside position=topright across=1;
title "Interaction Plot of Drug and Dosage (with Control Reference)";
run;
```

![Interaction plot for drug by dosage with control as reference](fact_blk3.png)