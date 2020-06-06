#!/usr/bin/env python
# coding: utf-8

# https://github.com/timeamagyar/kdd-cup-99-python/blob/master/kdd%20linear%20separability.ipynb
#
# I am following the some steps in that notebook
#
# professors reference: https://rawgit.com/mhahsler/Introduction_to_Data_Mining_R_Examples/master/chap2.html
#
#
# The plots will need imported libraries to improve their look, that where a lot of coding time will go into
#
# Downloaded the csv from: https://datahub.io/machine-learning/kddcup99



network_flows <- read.csv(file = 'kddcup99_csv.csv')


# In[3]:


head(network_flows)


# In[4]:


summary(network_flows)


# In[5]:


unique(network_flows["label"])


# In[6]:


lapply(network_flows, class)


# ### Get numerical columns (necessary for correlation finding and normalization)

# In[17]:


network_flows[sapply(network_flows,is.numeric)]


# In[18]:


scaled_network_flow_stats <- scale(network_flows[sapply(network_flows,is.numeric)])


# ### Correlation table

# In[19]:


cor(scaled_network_flow_stats)


# ### Histograms for numerical data types (normalized)

# In[27]:


for (col in 2:ncol(network_flows)) {
    if(class(network_flows[,col])=="numeric"){
        hist(network_flows[,col], breaks=200)

    }
}


# ### Barplots for integer data types

# In[13]:


for (col in 2:ncol(network_flows)) {
    if(class(network_flows[,col])=="integer"){
        barplot(network_flows[,col])

    }
}


# #### Reduce categories if necessary, definitely neceesary for classification or intracategory analysis
# Instead of using different dataframes can select multiple rows using filter and use stratified sampleing

# In[ ]:


normal <- network_flows[network_flows$label=="normal",]
neptune <- network_flows[network_flows$label=="neptune",]
smurf <- network_flows[network_flows$label=="smurf",]


# In[8]:


head(neptune)
neptune <- neptune[sample(nrow(neptune),3),]
# network_flows %>% filter(label == "normal")
#pipe to filter command, many ways for same thing


# In[10]:


# plot(neptune, col=normal$serror_rate)


# In[11]:


neptune[sample(nrow(neptune),3),]
