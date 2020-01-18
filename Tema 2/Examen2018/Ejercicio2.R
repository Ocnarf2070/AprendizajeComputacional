rm(list = ls())
library(rpart)
voteTraining<-read.csv("Examen2018/vote.data")
vote<-read.csv("Examen2018/voteTraining.test")
names(voteTraining)<-c("handicappedinfants",
               "waterprojectcostsharing",
               "adoptionofthebudgetresolution",
               "physicianfeefreeze",
               "elsalvadoraid",
               "religiousgroupsinschools",
              "antisatellitetestban",
               "aidtonicaraguancontra",
               "mxmissile",
               "immigration",
               "synfuelscorporationcutback",
               "educationspending",
               "superfundrighttosue",
               "crime",
               "dutyfreeexports",
               "exportadministrationactsouthafrica", "votation")

names(vote)<-c("handicappedinfants",
                       "waterprojectcostsharing",
                       "adoptionofthebudgetresolution",
                       "physicianfeefreeze",
                       "elsalvadoraid",
                       "religiousgroupsinschools",
                       "antisatellitetestban",
                       "aidtonicaraguancontra",
                       "mxmissile",
                       "immigration",
                       "synfuelscorporationcutback",
                       "educationspending",
                       "superfundrighttosue",
                       "crime",
                       "dutyfreeexports",
                       "exportadministrationactsouthafrica", "votation")

myFormula <- votation~handicappedinfants+waterprojectcostsharing+adoptionofthebudgetresolution+elsalvadoraid+religiousgroupsinschools+antisatellitetestban+aidtonicaraguancontra+mxmissile+immigration+synfuelscorporationcutback+educationspending+superfundrighttosue+crime+dutyfreeexports+exportadministrationactsouthafrica
VoteTree  <- rpart(myFormula,data = voteTraining)
plot(VoteTree)
text(VoteTree,use.n=TRUE)

opt       <- which.min(VoteTree$cptable[,"xerror"])
cp        <- VoteTree$cptable[opt, "CP"];cp
pVoteTree <- prune(VoteTree, cp = cp)
plot(pVoteTree)
text(pVoteTree,use.n=TRUE)

Prediccion    <- predict(VoteTree, newdata = vote, type = "class")  
MC            <- table(vote$votation,Prediccion)           
Aciertos      <- MC[1, 1] / (MC[1, 1] + MC[2, 1]);Aciertos
Prediccion    <- predict(pVoteTree, newdata = vote, type = "class")  
MC            <- table(vote$votation,Prediccion)           
Aciertos      <- MC[1, 1] / (MC[1, 1] + MC[2, 1]);Aciertos

#Es imposible encontrar otro arbol podado con mejor acurracy, ya que el que tenemos es el mejor que hay

myFormulaT <- votation~.
VoteTreeT  <- rpart(myFormulaT,data = voteTraining)
plot(VoteTreeT)
text(VoteTreeT,use.n=TRUE)
Prediccion    <- predict(VoteTreeT, newdata = vote, type = "class")  
MC            <- table(vote$votation,Prediccion)           
Aciertos      <- MC[1, 1] / (MC[1, 1] + MC[2, 1]);Aciertos

#Nos da que la variable principal del arbol es esa que hemos agregado y su prediccion es mejor a los anteriores que no tenia este valor.