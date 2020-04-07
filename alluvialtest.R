# alluvial daiagram
#install.packages(alluvial)
library(alluvial)

tit <- as.data.frame(Titanic, stringsAsFactors = FALSE)
head(tit)
glimpse(tit)

# Plain and simple
alluvial(  tit[,1:4],   freq   = tit$Freq  )

# or with bells
alluvial(tit[,1:4], 
         freq   = tit$Freq,
         col    = ifelse(tit$Survived == "Yes", "red", "blue"),
         border = ifelse(tit$Survived == "Yes", "blue", "grey"),
      #   hide   = tit$Freq < 100, #uncomplicate by ignoring small
         cex    = 0.7,    #font size
         alpha  = 0.4    #  , blocks=FALSE   # for merging; ? 
         # other options ...
        )


###############
pal <- c( "red", "green" , "blue", "yellow")  # goood
tit %>%    mutate( ss = paste(Survived, Sex),  k  = pal[ match(ss, sort(unique(ss))) ] ) -> tit
ggplot(data = as.data.frame(tit),
       aes(axis1 = Class, axis2 = Sex, axis3 = Age, y = Freq)) +
  scale_x_discrete(  limits = c("Class", "Sex", "Age"), expand = c(.1, .05)   ) +
  xlab("Demographic") +
  ggalluvial::geom_alluvium(aes(fill = Survived  ,
                             #   hide = tit$Freq < 10,
                                col = k,
                               #x border = k,
                               #x  blocks=FALSE,
                               #x ordering = list(   NULL,    NULL,  order(Age, Sex )   )
                                ) ) +  # the flow
  ggalluvial::geom_stratum(mapping = NULL, data = NULL, stat = "stratum", 
                           position = "identity", show.legend = NA, inherit.aes = TRUE, width = 1/3) +             # The bars
  geom_text(stat = "stratum", infer.label = TRUE) +
  theme_minimal() +
  ggtitle("passengers on  Titanic", "stratified by demographics and survival")



# simplest only two cross
# Survival status and Class
library(tidyverse)    # for %>%

tit %>% group_by(Class, Survived)  %>%
        summarise(  n = sum(Freq)  )        -> tit2d

alluvial(  tit2d[,1:2], freq=tit2d$n  )


#Three variables Sex, Class, and Survived:
    
# Survival status, Sex, and Class
tit %>% group_by(Sex, Class, Survived) %>%
        summarise(n = sum(Freq)) -> tit3d

alluvial(   tit3d[,1:3],    freq=tit3d$n)

# Customizing colors
# Colors of the alluvia can be customized with col, border and alpha arguments. For example:
    
alluvial(    tit3d[,1:3],   freq=tit3d$n,
             col = ifelse( tit3d$Sex == "Female",  "pink",  "lightskyblue"),
             border = "green",  
            # blocks=FALSE,
             alpha = 0.7
    )

# hide small items

alluvial(tit2d[,1:2], freq=tit2d$n, hide=tit2d$n < 150)

# This skips drawing the alluvia corresponding to the following rows in tit data frame:
tit2d %>% select(Class, Survived, n) %>% filter(n < 150)

#Changing “layers”
# By default alluvia are plotted in the same order in which the rows are ordered in the dataset.

d <- data.frame(
    x = c(1, 2, 3),
    y = c(3 ,2, 1),
    freq=c(1,1,1)
    )
d



#As there are three rows, we will have three alluvia:
    
alluvial(  d[,1:2],       freq=d$freq, col=1:3, alpha=1)
# Reversing the order
alluvial(  d[ 3:1, 1:2 ], freq=d$freq, col=3:1, alpha=1)

# or with single para
alluvial(d[,1:2], freq=d$freq, col=1:3, alpha=1,   layer=3:1)





pal <- c("blue", "lightskyblue4", "red", "yellow")  # goood

tit %>%    mutate(      ss = paste(Survived, Sex),
                        k  = pal[ match(ss, sort(unique(ss))) ]
                 ) -> tit
tit
alluvial(    tit[,c(4,2,3)],  freq=tit$Freq,
             hide = tit$Freq < 10,
             col = tit$k,
             border = tit$k,
             blocks=FALSE,
             ordering = list(   NULL,    NULL,  order(tit$Age, tit$Sex )   )
          )

#############################################
###############  vertical

library(ggalluvial)

ggplot(as.data.frame(Titanic),
       aes( y = Freq, 
            axis1 = Survived, 
            axis2 = Sex, 
            axis3 = Class)  ) +
    geom_alluvium( aes(fill = Class),
                   width = 0, 
                   knot.pos = 0,  
                   reverse = FALSE
                   ) +
    guides(fill = FALSE) +
    geom_stratum(width = 1/8, reverse = FALSE) +
    geom_text(stat = "stratum", label.strata = TRUE, reverse = FALSE) +
    scale_x_continuous(breaks = 1:3, labels = c("Survived", "Sex", "Class")) +
  #  coord_flip() +
    ggtitle("Titanic survival by class and sex")

###########
titanic_wide <- data.frame(Titanic)

ggplot(data = as.data.frame(Titanic),
        aes(axis1 = Class, axis2 = Sex, axis3 = Age, y = Freq)) +
        scale_x_discrete(  limits = c("Class", "Sex", "Age"), expand = c(.1, .05)   ) +
        xlab("Demographic") +
        geom_alluvium(aes(fill = Survived)) +
        geom_stratum() + 
        geom_text(stat = "stratum", infer.label = TRUE) +
        theme_minimal() +
        ggtitle("passengers on  Titanic", "stratified by demographics and survival")


# or
#The data is in “wide” format, but ggalluvial also recognizes data in “long” format and can convert between the two:
titanic_long <- to_lodes_form(data.frame(Titanic), key = "Demographic", axes = 1:3)
ggplot(data = titanic_long,
       aes(x = Demographic, stratum = stratum, alluvium = alluvium,
           y = Freq, label = stratum)) +
    geom_alluvium(aes(fill = Survived)) +
    geom_stratum() + geom_text(stat = "stratum") +
    theme_minimal() +
    ggtitle("passengers on Titanic", "stratified by demographics and survival")

#########################################

######

##########


library(networkD3)
nodes = data.frame("name" =    c("Node A",       # Node 0
                                "Node B",        # Node 1
                                "Node C",        # Node 2
                                "Node D",        # Node 3
                                "Nodes E"
                                ))       

links = as.data.frame(  matrix(
                                c(  0, 1, 5,           # Each row represents a link. The first number
                                    0, 2, 20,          # represents the node being conntected from. 
                                    1, 3, 30,          # the second number represents the node connected to.
                                    2, 3, 40,          # The third number is the value of the node
                                    3, 4, 25),
                                    byrow = TRUE, ncol = 3))

names(links) = c("source", "target", "value")

links

sankeyNetwork( Links = links, 
               Nodes = nodes,
               Source = "source", 
               Target = "target",
               Value = "value", 
               NodeID = "name",
               fontSize= 18, 
               nodeWidth = 30  )




######

data(majors)
# omit missing lodes and incident flows
ggplot(majors,
       aes(x = semester, stratum = curriculum, alluvium = student)) +
       geom_alluvium(fill = "darkgrey", na.rm = TRUE) +
       geom_stratum(aes(fill = curriculum), color = NA, na.rm = TRUE) +
       #scale_y_reverse() + 
       #geom_flow(stat = "alluvium", lode.guidance = "rightleft", color = "black")
       theme_bw()


## Not run: 
# Recreate Bostock Sankey diagram: http://bost.ocks.org/mike/sankey/
# Load energy projection data
URL <- paste0('https://cdn.rawgit.com/christophergandrud/networkD3/' , 'master/JSONdata/energy.json')
energy <- jsonlite::fromJSON(URL)
str(energy) # a List of 2
unnest(energy)
# Plot
sankeyNetwork(Links = energy$links, 
              Nodes = energy$nodes, 
              Source = 'source',
              Target = 'target', 
              Value = 'value', 
              NodeID = 'name',
              units = 'TWh', 
              fontSize = 12, 
              nodeWidth = 30)

# Colour links
energy$links$energy_type <- sub(' .*', '', energy$nodes[energy$links$source + 1, 'name'])

sankeyNetwork(  Links = energy$links, 
                Nodes = energy$nodes, 
                Source = 'source',
                Target = 'target', 
                Value = 'value', 
                NodeID = 'name',
                LinkGroup = 'energy_type', 
                NodeGroup = NULL             )




#########
# devtools::install_github("corybrunson/ggalluvial", ref = "optimization")
library(ggalluvial)

titanic_wide <- data.frame(Titanic)
ggplot(data = titanic_wide,
       aes(axis1 = Class, axis2 = Sex, axis3 = Age,    y = Freq)) +
    scale_x_discrete(limits = c("Class", "Sex", "Age"), expand = c(.1, .05)) +
    xlab("Demographic") +
    geom_alluvium(aes(fill = Survived)) +
    geom_stratum() + geom_text(stat = "stratum", label.strata = TRUE) +
    #coord_flip() +
    theme_minimal() +
    ggtitle("passengers on the maiden voyage of the Titanic",
            "stratified by demographics and survival") +
    theme(legend.position = 'bottom')

## 
ggplot(titanic_wide,
       aes( axis1 = Survived, axis2 = Sex, axis3 = Class,  y = Freq )) +
       geom_alluvium(aes(fill = Class), width = 0, knot.pos = 0, reverse = FALSE) +
       guides(fill = FALSE) +
       geom_stratum(width = 1/8, reverse = FALSE) +
       geom_text(stat = "stratum", label.strata = TRUE, reverse = FALSE) +
       scale_x_continuous(expand = c(0, 0), breaks = 1:3, labels = c("Survived", "Sex", "Class")) +
       scale_y_discrete(expand = c(0, 0)) +
       coord_flip() +
       ggtitle("Titanic survival by class and sex")


####
library(googleVis)

df=data.frame(country = c("US", "GB", "BR"),  val1 = c(10,13,14),   val2 = c(23,12,32)           )

Line <- gvisLineChart(df)
plot(Line)

Bubble <- gvisBubbleChart(Fruits, 
                          idvar="Fruit", 
                          xvar ="Sales", 
                          yvar="Expenses",
                          colorvar="Year", 
                          sizevar="Profit",
                          options=list( hAxis='{minValue:75, maxValue:125}')      )
plot(Bubble)
