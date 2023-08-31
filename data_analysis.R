# Read data from an online source into the 'flicker' data frame
flicker <- read.table(file="http://www.statsci.org/data/general/flicker.txt", header=TRUE)
flicker

# Extract 'Flicker' and 'Colour' columns from the 'flicker' data frame
Flicker <- flicker$Flicker
Colour <- flicker$Colour

# Calculate the Mean Square Error (MSE)
MSE = 2.3944

# Calculate the 97.5th percentile of the t-distribution
t97.5 = qt(.975, df=16)

# Count the sample sizes for each 'Colour' category
n = table(Colour)

# Calculate means for each 'Colour' category
means = by(Flicker, Colour, mean)

# Calculate differences between treatment means using the 'outer' function
# The outer function calculates the differences between all combinations of means
outer(means, means, "-")

# Calculate the Tukey's Honestly Significant Difference (HSD) using the formula
t97.5 * sqrt(MSE * outer(1/n, 1/n, "+"))

# Perform pairwise t-tests between groups to identify significant differences
pairwise.t.test(Flicker, Colour)

# Calculate the Tukey's HSD Critical Value for a confidence level of 0.95
qtukey(.95, nmeans=3, df=16)

# Fit a linear model of 'Flicker' on 'Colour'
L = lm(Flicker ~ Colour)

# Perform analysis of variance (ANOVA) on the linear model
anova(L)

# Fit an analysis of variance (ANOVA) model of 'Flicker' on 'Colour'
M = aov(Flicker ~ Colour)

# Display summary statistics of the ANOVA model
summary(M)

# Perform Scheffe's test for pairwise comparisons
library(DescTools)
ScheffeTest(model)

# Perform Dunnett's test for comparisons
DunnettTest(x, g)

# Perform Tukey's HSD test for pairwise comparisons
TukeyHSD(M)

# Plot Tukey's HSD confidence intervals
plot(TukeyHSD(M))

# Load necessary packages for multiple comparisons
library(multcomp)
library(multcompView)

# Perform multiple comparisons using generalized linear hypothesis tests
summary(glht(M, linfct=mcp(Colour="Tukey")))

summary(glht(M, linfct=mcp(Colour="Dunnet")))

# Read data from an online source into the 'dat' data frame
dat <- read.table(file="https://raw.githubusercontent.com/mariarizzo/RbyExample/master/Rx-data/SiRstv.txt", header=TRUE)
head(dat)

# Convert 'Instrument' column to a factor
dat$Instrument = as.factor(dat$Instrument)

# Display the structure of the 'dat' data frame
str(dat)

# Create a boxplot of 'Resistance' grouped by 'Instrument'
boxplot(Resistance ~ Instrument)

# Create a strip chart of 'Resistance' grouped by 'Instrument'
stripchart(Resistance ~ Instrument, vertical=TRUE)

# Calculate means and standard deviations of 'Resistance' by 'Instrument'
by(Resistance, Instrument, FUN=function(x) c(mean(x), sd(x)))

# Fit an analysis of variance (ANOVA) model of 'Resistance' on 'Instrument'
L = aov(Resistance ~ Instrument)

# Display summary statistics of the ANOVA model
summary(L)

# Create a 2x2 grid of residual plots for the ANOVA model
par(mfrow=c(2, 2))
plot(L)

# Restore display settings for plotting
par(mfrow=c(1, 1))

# Create a Tukey's HSD plot for the ANOVA model
plot(TukeyHSD(L))

# Read data from another online source into the 'times' data frame
file1 = "http://www.itl.nist.gov/div898/strd/anova/SiRstv.dat"
dat = read.table(file1, skip=60)

# Read data from a tab-separated file into the 'times' data frame
times = read.table("PATIENT.DAT", sep="\t")
names(times) = c("stomach","bronchus","colon","ovary","breast")

# Stack columns of the 'times' data frame into rows
times1 = stack(times)
names(times1) = c("time", "organ")

# Remove rows with missing values
times1 = na.omit(times1)

# Display the first 3 rows of 'times1'
head(times1, 3)

# Display the last 3 rows of 'times1'
tail(times1, 3)

# Check if 'organ' column is a factor
is.factor(times1$organ)

# Display summary statistics of the 'times1' data frame
summary(times1)

# Read data from a CSV file into the 'times' data frame
times = read.csv("PATIENT.csv")
head(times)
