# Biostats Final project
## Brief introductions
This dataset is from the opensource website dryad. The dataset I will be looking into is about the effects of oxytocin receptor genes on dogs during drug detection training. This research was published in the journal of heredity by Akitsugu Konno and other researchers. The biological drive for this research was mainly to see the connection between genetic polymorphism(genetic differences) and how it impacts the dog’s trainability.  Genetic polymorphism in oxytocin receptors is also closely related to dog and human relationships which affect training. For that reason, researchers also looked at kept personality scores which were reported by the trainers. Overall, this study compares three oxytocin receptor (OXTR) genotypes compared to dog training’s success on 340 labrador retrievers. 

## Descriptions of  Data:
The study was conducted on 340 labrador retrievers from the Canine Training Center, Tokyo. The data is independent and random sampled as the dogs were not genetically related to one another. This study is an observational study as the researchers simply observed whether or not a dog is trainable during the drug detection training.  The sample of 340 labrador retrievers, roughly 140 were female and 200 were male, sex was also included in the analyses. Another major aspect of the study was personality scores reported by dog trainers. Personality was split into 7 items (Concentration, Interest in dummy, Activity, Friendliness to humans, Boldness, Independency, and tolerance to dogs). The personality score ranges from 1-5, 1 being “very low” and 5 being “very high”.  These 7 personality items are put into two categories, factor 1(training focus) and factor2(Tolerance). Of the 7 personality items; concentration, interest in the dummy, activity, friendliness to humans, and boldness are classified in Factor1(training focus). While tolerance and independency are classified in factor2(Tolerance). These 7 items were considered ideal personality items needed for a trainable dog to succeed. The study also had multiple different explanatory variables(sex, oxytocin receptor genotype, factor1 &factor2) and one response variable(qualification).  The three genotypes of oxytocin receptors(OXTR) were GTT/GTT, GTC/GTT, and GTC/GTC.  

## Statistical hypotheses:
The null hypothesis is that the genotype difference in oxytocin receptors(OXTR) and personality(factor1 & factor2) has no effect on the dog’s success in drug detection training. The alternative hypothesis is that genotype in oxytocin receptors and personality have an effect on a dog’s success in drug detection.  The underlying biological hypothesis is that the polymorphism of oxytocin receptors can make a dog more successful during training. Appropriate statistical tests to test the null would be logistic regression. 

## Statistical approach:

I conducted a logistic regression along with an ANOVA chi2test. We assume that the data is unbiased and independent. We assume that the variance is independent of predictors and that error is normally distributed. GLMS are used instead of a linear model when the data breaks assumptions like binomial and count data. This dataset does meet logistic regression assumptions of data being independent and unbiased. It also meets that the error was normally distributed. I conducted a logistic regression by putting all the explanatory variables and the response variable(qualification) into one model. 
Another major aspect of the dataset was personality and I used a Principal Component Analysis (PCA). I conducted a PCA to get a better image of how personality affects the qualification of the dog.  The PCA allowed me to get a better understanding of variation amongst the personality items. The PCA looked at the 7 personality items and saw how which items affect qualification more. The principal component analysis allowed me to get a better understanding of the dataset and how personality affected the trainability of the dogs. The researchers originally used a different type of statistical test which does the same thing as a PCA. 

##  Results:
I conducted a logistic regression with four explanatory variables (sex, factor1, factor2, and genotype) on the dog’s trainability(qualification).  In order, to find a p-value and deviance, I used the full model and an ANOVA(chi2test). Looking at table 1, with the logistic regression only a few of the explanatory variables actually have a significant effect. Genotype has a significant effect on a dog's trainability. We reject the null hypothesis, so genotype has a significant effect.  More specifically genotype T/C has the highest effect on a  dog’s qualification during training figure 1 is also able to portray it. This maps back to the biological drive as polymorphism does indeed affect the dog’s trainability. Factor 1(Training focus) also has a significant effect on the dog’s trainability. The parameter estimate of  factor1 was 1.92. This is due to the fact that the p-value is less than alpha, which means we reject the null hypothesis. So dogs that have higher training focus tend to qualify during training. Both sex and factor2 didn’t have enough evidence to reject the null. We fail to reject the null for sex with a parameter estimate of 0.54 for the male sex. Factor2(tolerance) had a parameter estimate of 0.00437.  We also fail to reject the null for factor2 since it's much larger than alpha(a= 0.05). 


<img width="642" alt="Screen Shot 2020-11-29 at 5 10 51 PM" src="https://user-images.githubusercontent.com/75222465/100556208-52ac2b00-3266-11eb-9059-6d1e4d73fe29.png">


<img width="295" alt="Screen Shot 2020-11-29 at 5 10 27 PM" src="https://user-images.githubusercontent.com/75222465/100556200-41631e80-3266-11eb-8ed3-e4b359280a05.png">


A PCA was conducted on the 7 personality items which found the variability of personality. In figure 5, all 7 personality items can be seen along with their relationship to qualification. Higher PC2 is closely related to tolerance to dogs the more independent the dog lower PC2. While the other 5 personality items(concentration, interest in dummy, friendliness to humans, boldness & activity) have a directly proportional relationship with PC1. So, in fact, PC1 and factor1 are indeed the exact same and the same goes for PC2 and factor2. The figure also shows that higher PC1 also presents that dogs tend to receive qualification during training. Figure 3 shows that PC1 has a larger effect on the qualification than even genotype. PC1 is mainly training focus of the dog which makes sense that a more focused dog will qualify. With figure 3 you can see that with an increase of PC1(training focus) a dog is much more trainable and qualifies. 

<img width="451" alt="Screen Shot 2020-11-29 at 5 16 01 PM" src="https://user-images.githubusercontent.com/75222465/100556248-9ef76b00-3266-11eb-925d-3d448df40ab0.png">
<img width="407" alt="Screen Shot 2020-11-29 at 5 16 13 PM" src="https://user-images.githubusercontent.com/75222465/100556250-a28af200-3266-11eb-80d3-c0d8823fa73c.png">

## Conclusion:
These results relate to the biological question polymorphism of oxytocin receptor effect on qualification. The results conclude that dogs with genotype T/C have a higher tendency to qualify during training. The other aspect was the effect of oxytocin receptors on the social behaviors of dogs. The results present that dogs with higher training focus(factor1) tend to qualify more. Statistically the results show that of the four explanatory variables only genotype and factor1 had an impact on how trainable a dog is. Conclusions we can make from this study is that polymorphism of the oxytocin receptor does have an effect on dog qualification during drug detection training. Also, that training focus(factor1) has a huge impact on the trainability of a dog regardless of genotype.   One caveat is that one should assume that if the dog has the T/C genotype and higher training focus it doesn’t automatically mean they will qualify. 
