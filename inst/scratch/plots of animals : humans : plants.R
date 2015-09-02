

things <- data.frame(sosid, humans, animals, plants)
library(reshape2)
things_melted <- melt(things, id.vars = "sosid")
qplot(x = value, fill = variable, data = things_melted, position = "fill")
qplot(x = value, fill = variable, data = things_melted, position = "dodge")
qplot(x = value, fill = variable, data = things_melted, position = "identity", alpha = 0.3)
