# add library to compute Plackett-Luce worth per fragment
library(PlackettLuce)
library(car)

### READ IN USER STUDY DATA ORIGINAL EXPERIMENT
# read in file, while using second row as header
file = "~/user-study-results/original_user_study.csv"
results <- read.csv(file, skip=1, header=TRUE, stringsAsFactors = FALSE)

# remove the last header
results <- results[-1,]

# remove replies obtained during testing and those that did not agree with informed consent
results <- results[results$Distribution.Channel == "anonymous",] 
results <- results[results$Als.u.deel.wilt.nemen.aan.het.onderzoek..vraag.ik.u.om.de.toestemmingsverklaring.hieronder.goed.te.lezen.en.te.bevestigen.dat.u.toestemming.geeft..Toestemmingsverklaring...Ik.ben.naar.tevredenheid.over.het.onderzoek.geïnformeerd..Ik.heb.de..mogelijkheid.gekregen.om.vragen.over.het.onderzoek.te.stellen.en..eventuele.vragen.zijn.naar.tevredenheid.beantwoord..Ik.heb.over.deelname..aan.het.onderzoek.kunnen.nadenken..Ik.begrijp.dat.het.mij.vrij.staat.om..op.elk.gewenst.moment.het.experiment.af.te.breken..Ik.begrijp.dat.er..voor.mij.geen.risico.s.of.ongemakken.te.verwachten.zijn.op.basis.van..mijn.deelname.aan.dit.experiment..Ik.begrijp.dat.de.anonieme.data.die..met.dit.experiment.verzameld.wordt..elektronisch.opgeslagen.zal.worden...Ik.begrijp.dat.de.verzamelde.data.zal.worden.gebruikt.voor..wetenschappelijke.doeleinden.en.eventueel.zal.worden.gepubliceerd..Ik..geef.hierbij.uit.vrije.wil.toestemming.om.deel.te.nemen.aan.het..onderzoek..Ten.slotte.bevestig.ik.dat.ik.18.jaar.of.ouder.ben. == "Ik geef toestemming",]

# remove unimportant columns
results$Start.Date <- NULL
results$End.Date <- NULL
results$Response.Type <- NULL
results$IP.Address <- NULL
results$Recorded.Date <- NULL
results$Response.ID <- NULL
results$Recipient.Last.Name <- NULL
results$Recipient.First.Name <- NULL
results$Recipient.Email <- NULL
results$External.Data.Reference <- NULL
results$Location.Latitude <- NULL
results$Location.Longitude <- NULL
results$Distribution.Channel <- NULL
results$User.Language <- NULL
results$Als.u.deel.wilt.nemen.aan.het.onderzoek..vraag.ik.u.om.de.toestemmingsverklaring.hieronder.goed.te.lezen.en.te.bevestigen.dat.u.toestemming.geeft..Toestemmingsverklaring...Ik.ben.naar.tevredenheid.over.het.onderzoek.geïnformeerd..Ik.heb.de..mogelijkheid.gekregen.om.vragen.over.het.onderzoek.te.stellen.en..eventuele.vragen.zijn.naar.tevredenheid.beantwoord..Ik.heb.over.deelname..aan.het.onderzoek.kunnen.nadenken..Ik.begrijp.dat.het.mij.vrij.staat.om..op.elk.gewenst.moment.het.experiment.af.te.breken..Ik.begrijp.dat.er..voor.mij.geen.risico.s.of.ongemakken.te.verwachten.zijn.op.basis.van..mijn.deelname.aan.dit.experiment..Ik.begrijp.dat.de.anonieme.data.die..met.dit.experiment.verzameld.wordt..elektronisch.opgeslagen.zal.worden...Ik.begrijp.dat.de.verzamelde.data.zal.worden.gebruikt.voor..wetenschappelijke.doeleinden.en.eventueel.zal.worden.gepubliceerd..Ik..geef.hierbij.uit.vrije.wil.toestemming.om.deel.te.nemen.aan.het..onderzoek..Ten.slotte.bevestig.ik.dat.ik.18.jaar.of.ouder.ben. <- NULL
results$Wilt.u.op.de.hoogte.blijven.van.de.resultaten.van.dit.onderzoek..Dan.kunt.u.een.link.naar.de.masterscriptie.ontvangen....Selected.Choice <- NULL
results$Q_TerminateFlag <- NULL
results$Finished <- NULL
results$Progress <- NULL
results$Duration..in.seconds. <- NULL
results$Wilt.u.op.de.hoogte.blijven.van.de.resultaten.van.dit.onderzoek..Dan.kunt.u.een.link.naar.de.masterscriptie.ontvangen....Ja..e.mail....Tekst <- NULL

# transform "yes" and "no" into logical values
for (i in names(results)){
  for (j in 1:nrow(results)) {
    if (results[j, i] == "Ja"){
      results[j, i] <- as.logical(TRUE)
    }else if (results[j, i] == "Nee"){
      results[j, i] <- as.logical(FALSE)
    }
  }
}

# for-loop to compute counts, scores, and Plackett-Luce worth per fragment per song
scores <- data.frame(segment.id=character(), count=double(), familiarity=double(), worth=double(), logworth=double(), stringsAsFactors=FALSE) 
counter = 0 
for (i in names(results)){
  # obtain name of fragment
  partname <- sub(".+\\.\\.+", "", i)
  
  # check whether results belong to the same fragment as the previous, otherwise restart temporary data frame
  if (counter %% 7 == 0){
    dfname <- sub("\\.\\.+.+", "", i)
    dfname <- sub("\\.", "-", dfname)
    tempdf <- data.frame(as.double(results[,i]))
    names(tempdf) <- c(paste0(dfname, "-", partname))
    
    # check whether familiarity is asked, which also is the last question per song
  } else if (sub("\\.\\.+.*", "", i) == "Kent.u.het.nummer" || sub("\\.\\.+.*", "", i) == "Kent.u.het.nummer.") {
    #helpdf <- tempdf # also only needed for creating separate data frames per song
    tempdf <- cbind(tempdf, as.logical(results[,i]))
    
    # if uncommented, creates separate data frames per song with results survey
    #names(tempdf) <- c(names(helpdf), paste0(dfname, "-", "familiarity"))
    #assign(dfname, tempdf)
    
    # add computed mean and count how often each fragment is rated
    tempScores <- as.data.frame(colSums(!is.na(tempdf[,1:6])))
    
    # compute familiarity score with log((familiar + 1)/(unfamiliar + 1))
    familiar <- nrow(tempdf[tempdf[,7] == TRUE,])
    unfamiliar <- nrow(tempdf[tempdf[,7] == FALSE,])
    tempFamiliarity <- data.frame(rep(log((familiar + 1)/(unfamiliar + 1)), 6))
    
    # add Plackett-Luce worth
    tempdf <- tempdf[rowSums(is.na(tempdf)) != ncol(tempdf),]
    tempdf[is.na(tempdf)] <- 0
    tempdf <- aggregate(list(numdup=rep(1, nrow(tempdf))), tempdf[,1:6], length)
    R <- as.rankings(tempdf[,1:6])
    
    PLmodel <- PlackettLuce(R, weights = tempdf$numdup)
    PLscores <- as.data.frame(coef(PLmodel, log = FALSE))
    
    # normalised log values of worth
    PLlog <- as.data.frame(log(PLscores))
    
    # bind the different scores
    tempScores <- cbind(rownames(tempScores), tempScores, tempFamiliarity, PLscores, PLlog)
    names(tempScores) <- c("segment.id", "count", "familiarity", "worth", "logworth")
    scores <- rbind(scores, tempScores)
    
    # add fragment results to temporary dataframe
  } else {
    helpdf <- tempdf
    tempdf <- cbind(tempdf, as.double(results[,i]))
    names(tempdf) <- c(names(helpdf), paste0(dfname, "-", partname)) 
  }
  counter = counter + 1
}

scores$scaleworth <- scale(scores$logworth)
scores$familiarity <- scale(scores$familiarity)
scores$type <- factor(ifelse(sub(".+-", "", scores$segment.id) == "min", "MIN", ifelse(sub(".+-", "", scores$segment.id) == "random", "RANDOM", "MSAF")))


### READ IN USER STUDY DATA REPLICATION STUDY
# Read in the files with user study results
testfile = "~/user_study_results/replication_user_study/21Guns.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- as.data.frame(coef(testPLmodel, log = FALSE))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- as.data.frame(colMeans(computeMean, na.rm=TRUE))
# add computed mean and count how often each fragment is rated
testCount <- as.data.frame(colSums(!is.na(computeMean[,1:6])))

testfile = "~/user_study_results/replication_user_study/Albatross.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile = "~/user_study_results/replication_user_study/Alone.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile = "~/user_study_results/replication_user_study/Annie'ssong.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile= "~/user_study_results/replication_user_study/Batoutofhell.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile = "~/user_study_results/replication_user_study/Belfastchild.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile = "~/user_study_results/replication_user_study/Bestofyou.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile = "~/user_study_results/replication_user_study/Blowin'inthewind.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile = "~/user_study_results/replication_user_study/Borntobewild.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile = "~/user_study_results/replication_user_study/DoIwannaknow.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile= "~/user_study_results/replication_user_study/Don'tletthesungodownonme.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile = "~/user_study_results/replication_user_study/Duhast.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile = "~/user_study_results/replication_user_study/Gangsta'sparadise.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile = "~/user_study_results/replication_user_study/Hallelujah.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile = "~/user_study_results/replication_user_study/Haltmich.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile = "~/user_study_results/replication_user_study/Hometownglory.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile = "~/user_study_results/replication_user_study/Jungleland.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile = "~/user_study_results/replication_user_study/Lopentotdezonopkomt.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile = "~/user_study_results/replication_user_study/Mammamia.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testMiddleCount <- as.data.frame(colSums(!is.na(computeMean[,1:5])))
names(testMiddleCount) <- names(testCount)
testCount <-rbind(testCount, testMiddleCount)

testfile = "~/user_study_results/replication_user_study/Myimmortal.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile = "~/user_study_results/replication_user_study/Purplerain.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile = "~/user_study_results/replication_user_study/Respect.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile = "~/user_study_results/replication_user_study/Silentlucidity.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile = "~/user_study_results/replication_user_study/Sterrenstof.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile = "~/user_study_results/replication_user_study/Thescientist.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile = "~/user_study_results/replication_user_study/Theunforgiven.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile = "~/user_study_results/replication_user_study/ThewindcriesMary.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile = "~/user_study_results/replication_user_study/Walkoflife.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile = "~/user_study_results/replication_user_study/Welterustenmijnheerdepresident.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile = "~/user_study_results/replication_user_study/Wherethewildrosesgrow.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile = "~/user_study_results/replication_user_study/Wickedgame.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

testfile = "~/user_study_results/replication_user_study/Wishyouwerehere.csv"
testsecond <- as.data.frame(table(read.csv(testfile, header=FALSE, stringsAsFactors = FALSE)))
testtable <- testsecond[which(testsecond$Freq > 0),]
testR <- as.rankings(testtable[,1:3], input = "ordering")
testPLmodel <- PlackettLuce(testR, weights = testtable$Freq)
testPLscores <- rbind(testPLscores, as.data.frame(coef(testPLmodel, log = FALSE)))
computeMean <- as.data.frame(testR[1:length(testR), as.rankings=FALSE])
is.na(computeMean) <- computeMean == 0
computeMeans <- rbind(computeMeans, as.data.frame(colMeans(computeMean, na.rm=TRUE)))
# add computed mean and count how often each fragment is rated
testCount <- rbind(testCount, as.data.frame(colSums(!is.na(computeMean[,1:6]))))

# normalised log values of worth
testPLlog <- as.data.frame(log(testPLscores$`coef(testPLmodel, log = FALSE)`))

# bind the different scores
scores2 <- cbind(rownames(testPLscores), testCount, testPLscores, testPLlog, computeMeans)
names(scores2) <- c("segment.id", "count", "worth", "logworth", "meanscore")
scores2$segment.id <- sub(".mp3", "", scores2$segment.id)
scores2$scaleworth <- scale(scores2$logworth)

### READ IN CATCHY FEATURES ORIGINAL STUDY
featsfile = "~/catchy_features+matlab.csv"
feats <- read.csv(featsfile, header=TRUE, stringsAsFactors = FALSE)

# first ensure columns are numeric and make data frame usable
dfFeats <- data.frame(as.double(feats$chromahist3.normentropy.minlog))
names(dfFeats) <- c("chromahist3.normentropy.minlog")
for(i in names(feats[, 2:45])){
  dfFeats <- cbind(dfFeats, as.double(feats[, i]))
}
names(dfFeats) <- names(feats[,1:45])
dfFeats$segment.id <- NULL


### READ IN CATCHY FEATURES REPLICATION STUDY
featsfile2 = "~/catchy_features_second_all_segments+matlab.csv"
feats2 <- read.csv(featsfile2, header=TRUE, stringsAsFactors = FALSE)

# first ensure columns are numeric and make data frame usable
dfFeats2 <- data.frame(as.double(feats2$chromahist3.normentropy.minlog))
names(dfFeats2) <- c("chromahist3.normentropy.minlog")
for(i in names(feats2[, 2:45])){
  dfFeats2 <- cbind(dfFeats2, as.double(feats2[, i]))
}
names(dfFeats2) <- names(feats2[,1:45])
dfFeats2$segment.id <- NULL

### FACTOR ANALYSIS OVER FEATURE BOTH STUDIES COMBINED
features <- rbind(feats, feats2)
dfFeatures <- rbind(dfFeats, dfFeats2)

# exploratoy factor analysis
library(psych)
# library for rotations in psych-package
library(GPArotation)

# compute Spearman correlation matrix
corFeatures <- cor(dfFeatures, method="spearman")

# minimum residuals factor analysis
minresBoth <- fa(corFeatures, 5, rotate="Varimax")
fa.sort(minresBoth)

# create a latex template of the factor model
fa2latex(fa.sort(minresBoth))

# factor analysis scores
minresScoresBoth <- factor.scores(dfFeatures, minresBoth)
minresScoresBoth <- minresScoresBoth$scores
faFeatures <- data.frame(segment.id = features$segment.id, minresScoresBoth)

# merge the computed scores, worth, and count with features file
scores$familiarity <- NULL
scores$type <- NULL
scores$experiment <- 'original'
scores2$experiment <- 'replication'
scores2$meanscore <- NULL
both <- rbind(scores, scores2)
both$count <- NULL
both$worth <- NULL
both$logworth <- NULL
both$type <- factor(ifelse(sub(".+-", "", both$segment.id) == "min", "MIN", ifelse(sub(".+-", "", both$segment.id) == "random", "RANDOM", "MSAF")))

both$test <- list(experiment = 0.5 * contr.sum(2))

### GLM BASED ON BOTH STUDIES
combined_data <- merge(both, faFeatures)
combined_data_model <- glm(scaleworth ~ experiment * (MR1 + MR2 + MR3 + MR4 + MR5 + type), 
                           data = combined_data, 
                           contrasts = list(experiment = 0.5 * contr.sum(2), type = contr.Sum(levels(combined_data$type))))

summary(combined_data_model)

# test effect of type
Anova(combined_data_model)

# emmeans: double-check the random segmentation calculations
library(emmeans)
emmeans(combined_data_model, ~type * experiment)
