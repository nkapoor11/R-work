# these are the KTC y values of growth rate relative to inert plot for biological replicates
Stk11Ori <- c(1.0408171, 1.0827207, 1.1524777, 1.2622981, 1.3420774, 1.3846617, 1.3876971)
Stk11V1 <- c(1.0831623, 1.1242450, 1.2279257, 1.3161775, 1.3975882, 1.4104324, 1.4034163)
Setd2Ori <- c(1.0399436, 1.0826985, 1.1434563, 1.2431996, 1.3131571, 1.3238370, 1.2697713)
Setd2V1 <- c(1.0127442, 1.0300152, 1.0561375, 1.0997324, 1.1931350, 1.2159344, 1.2365827)

cor.test(Stk11Ori, Stk11V1)
cor.test(Setd2Ori, Setd2V1)

Neo1Ori <- c(1.0019989, 0.9983213, 1.0025919, 1.0128661, 1.0172665, 1.0130550, 1.0384337)
Neo2Ori <- c(1.0009466, 0.9995487, 1.0014816, 1.0018887, 1.0009157, 0.9981743, 0.9705663)
Neo3Ori <- c(0.9596724, 0.9251713, 0.8722807, 0.7941584, 0.7485978, 0.7456456, 0.7583954)
NT1Ori <- c( 0.9990543, 1.0004515, 0.9985206, 0.9981148, 0.9990851, 1.0018290, 1.0303263)
# p53 <- c(0.9648355, 0.9301720, 0.8824728, 0.8114845, 0.7670255, 0.7809406, 0.8378205)
rb1 <- c(1.0125325, 1.0105093, 1.0234529, 1.0459908, 1.0727047, 1.0837522, 1.1354309)
# Cdkn2aV1 <- c(1.0033243, 1.0012511, 1.0094198, 1.0237264, 1.0397578, 1.0482753, 1.1181141)
AtmV1 <- c(0.9968146, 0.9948782, 0.9991587, 1.0037358, 1.0105295, 1.0159809, 1.0219669)
Arid1aV1 <- c(1.0100163, 1.0117091, 1.0117727, 1.0224895, 1.0229347, 1.0268282, 1.0385492)
# Keap1 <- c(0.9952760, 0.9916632, 0.9947912, 1.0030358, 1.0113320, 1.0067269, 1.0252091)
# Smad4V2 <- c(1.0136184, 0.9836561, 0.9916186, 0.9989317, 1.0037324, 1.0128845, 0.9947135)


# KT relative growth rate vectors
Neo1OriKT <- c(0.9862546, 0.9881478, 0.9949999, 0.9859550, 0.9943823, 0.9924616, 0.9706915)
Neo2OriKT <- c(0.9995596, 1.0003008, 1.0040179, 1.0015141, 1.0157389, 1.0329401, 1.0674084)
Neo3OriKT <- c(1.0131707, 1.0163772, 1.0089532, 1.0137172, 1.0028378, 0.9993061, 0.9878615)
NT1OriKT <- c(1.0004406, 0.9996993, 0.9959981, 0.9984882, 0.9845049, 0.9681104, 0.9368486)
Stk11OriKT <- c(1.0030830, 1.0016860, 1.0103609, 1.0139075, 1.0054241, 1.0337172, 1.0098770)
Stk11V1KT <- c(1.0043524, 1.0049182, 1.0036593, 1.0240690, 1.0166459, 1.0113719, 1.0045066)
Setd2OriKT <- c(1.0007563, 0.9956878, 0.9961069, 1.0005208, 1.0028036, 1.0082953, 1.0428521)
Setd2V1KT <- c(1.0065421, 1.0048541, 1.0196302, 1.0042621, 0.9778398, 0.9583316, 0.9122243)
rb1KT <- c(0.9825965, 0.9804311, 0.9850553, 0.9921700, 0.9959284, 0.9778486, 1.0125249)
AtmV1KT <- c(0.9827740, 0.9849889, 0.9894981, 0.9855435, 0.9876949, 0.9708328, 0.9702919)
Arid1aV1KT <- c(0.9748217, 0.9759557, 0.9850449, 0.9848324, 0.9936537, 0.9883218, 0.9490156)

# mean values from google doc: 1.290197 1.336272 1.254491 1.168874 1.099526 1.048248 1.063305
# order of x/y values: Stk11Ori, Stk11V1, Setd2Ori, Setd2V1, Rb1Ori, AtmV1, Arid1aV1
# x <- c(1.012589, 1.011359, 1.008196, 0.9846671, 0.9909408, 0.9830329, 0.9801573) # KT

# take mean of above 4 inerts to one inert
inert <- (Neo1Ori + Neo2Ori + Neo3Ori + NT1Ori)/4 # KTC inert
inertKT <- (Neo1OriKT + Neo2OriKT + Neo3OriKT + NT1OriKT)/4 # KT inert

# mean relative to inert KTC
r <- mean(Stk11Ori/inert) # 1.473552 for 95th percentile
print(r) # 1.238143
# get p value of mean relative to inert
cor.test(inert, Stk11Ori)

# mean relative to inert KT
r <- mean(Stk11OriKT/inertKT) # 1.473552 for 95th percentile
print(r) # 1.012589
# get p value of mean relative to inert
# cor.test(inert, Stk11Ori)



# mean relative to inert KTC
r <- mean(Stk11V1/inert) 
print(r)
# get p value of mean relative to inert
# print(cor.test(inert, Stk11V1))

# mean relative to inert KT
r <- mean(Stk11V1KT/inertKT) 
print(r)
# get p value of mean relative to inert
# print(cor.test(inert, Stk11V1))



# mean relative to inert # KTC
r <- mean(Setd2Ori/inert) 
print(r)
# get p value of mean relative to inert
print(cor.test(inert, Setd2Ori))

# mean relative to inert # KT
r <- mean(Setd2OriKT/inertKT) 
print(r)
# get p value of mean relative to inert
# print(cor.test(inert, Setd2OriKT))


# mean relative to inert KTC
r <- mean(Setd2V1/inert) 
print(r)
# get p value of mean relative to inert
# print(cor.test(inert, Setd2V1))

# mean relative to inert KT 
r <- mean(Setd2V1KT/inertKT) 
print(r)
# get p value of mean relative to inert
# print(cor.test(inert, Setd2V1))



# mean relative to inert KTC
r <- mean(rb1/inert) 
print(r)
# get p value of mean relative to inert
# print(cor.test(inert, rb1))

# mean relative to inert KT
r <- mean(rb1KT/inertKT) 
print(r)
# get p value of mean relative to inert
# print(cor.test(inert, rb1))



# mean relative to inert KTC
r <- mean(AtmV1/inert) 
# print(AtmV1/inert)
print(r)
# get p value of mean relative to inert
# print(cor.test(inert, AtmV1))

# mean relative to inert KT
r <- mean(AtmV1KT/inertKT) 
# print(AtmV1/inert)
print(r)
# get p value of mean relative to inert
# print(cor.test(inert, AtmV1))



# mean relative to inert KTC 
r <- mean(Arid1aV1/inert) 
# print(Arid1aV1/inert)
print(r)
# get p value of mean relative to inert
print(cor.test(inert, Arid1aV1))

# mean relative to inert KT
r <- mean(Arid1aV1KT/inertKT) 
# print(Arid1aV1/inert)
print(r)
# get p value of mean relative to inert
# print(cor.test(inert, Arid1aV1))


# # mean relative to inert. p value to high, 0.6641 
# r <- mean(Smad4V2/inert) 
# print(Smad4V2/inert)
# print(r)
# # get p value of mean relative to inert
# print(cor.test(inert, Smad4V2))

# # mean relative to inert. p value too high, 0.06669 
# r <- mean(Keap1/inert) 
# print(Keap1/inert)
# print(r)
# # get p value of mean relative to inert
# print(cor.test(inert, Keap1))

# # mean relative to inert...p value is too high, 0.1359
# r <- mean(Cdkn2aV1/inert) 
# print(r)
# # get p value of mean relative to inert
# print(cor.test(inert, Cdkn2aV1))


# # mean relative to inert 
# r <- mean(p53/inert) 
# print(r)
# # get p value of mean relative to inert
# print(cor.test(inert, p53))

