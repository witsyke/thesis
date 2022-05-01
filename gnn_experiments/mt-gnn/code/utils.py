import torch
import networkx as nx
import numpy as np
import scipy.sparse as sp
import pandas as pd
from math import ceil
import glob
import unidecode 
from datetime import date, timedelta

from sklearn import preprocessing

import os
    
    
    
def read_meta_datasets(window, dataset, lockdown, cases):
    os.chdir("../data")
    # TODO i dont think I need these lists (only do it for Germany )
    meta_labs = []
    meta_graphs = []
    meta_features = []
    meta_y = []
    lockdown_indicator = None

      #------------------------- Germany
    os.chdir("Germany")
    print("LOADING GERMANY")
    labels = pd.read_csv(f"germany_labels_{cases}.csv")

    labels = labels.set_index("name")

    

    sdate = date(2020, 2, 24)
    edate = date(2020, 12, 31)
    #--- series of graphs and their respective dates
    delta = edate - sdate
    dates = [sdate + timedelta(days=i) for i in range(delta.days+1)]
    dates = [str(date) for date in dates]
    
    labels = labels.loc[:,dates]    #labels.sum(1).values>10

    graph_list = generate_graphs_tmp(dates,"GER", dataset)
    gs_adj = [nx.adjacency_matrix(kgs).toarray().T for kgs in graph_list]
    
    labels = labels.loc[list(graph_list[0].nodes()),:] # This filters all counties with data that are not in the movement graph

    if lockdown:
        lockdown_indicator = pd.read_csv(f"germany_lockdown_{lockdown}.csv")
        lockdown_indicator = lockdown_indicator.set_index("name")
        lockdown_indicator = lockdown_indicator.loc[:,dates]
        lockdown_indicator = lockdown_indicator.loc[list(graph_list[0].nodes()),:]
        

   
    meta_labs.append(labels)

    meta_graphs.append(gs_adj)
    
    features = generate_new_features(graph_list ,labels , dates ,window, lockdown_indicator = lockdown_indicator, lockdown = lockdown)


    meta_features.append(features)

    y = list()
    for i,G in enumerate(graph_list):
        y.append(list())
        for node in G.nodes():
            y[i].append(labels.loc[node,dates[i]])

    meta_y.append(y)

    
    os.chdir("../../code")

    return meta_labs, meta_graphs, meta_features, meta_y
    
    
# TODO need to change this to use the node_labels
def generate_graphs_tmp(dates,country, dataset):
    graph_list = []

    if dataset in ["normal", "binary", "scaled_amount", "scaled_structure"]:
        for date in dates:
            d = pd.read_csv(f"graphs_{dataset}/{country}_{date}.csv",header=None)
            # print(d.dtypes)
            G_t = nx.DiGraph()
            # Extract all nodes from adjacency list
            nodes = set(d[0].unique()).union(set(d[1].unique()))
            G_t.add_nodes_from(nodes)

            # Iterate over all edges in the list and add them to the graph
            for row in d.iterrows():
                G_t.add_edge(row[1][0], row[1][1], weight=row[1][2])
            # print(G_t.nodes())
            graph_list.append(G_t)

    elif dataset == "distance":
        d = pd.read_csv(f"graphs_{dataset}/{country}_distance.csv",header=None)
        # Graph can be constructed outside of loop as it does not change
        G_t = nx.DiGraph()
        # Extract all nodes from adjacency list
        nodes = set(d[0].unique()).union(set(d[1].unique()))
        G_t.add_nodes_from(nodes)

        # Iterate over all edges in the list and add them to the graph
        for row in d.iterrows():
            G_t.add_edge(row[1][0], row[1][1], weight=row[1][2])

        for date in dates:
            graph_list.append(G_t)

    elif dataset in ["only_local", "full"]:
        d = pd.read_csv(f"graphs_distance/{country}_distance.csv",header=None)
           # Extract all nodes from adjacency list - I use the distances file here as a trick to get the nodes
        nodes = set(d[0].unique()).union(set(d[1].unique()))
        
        if dataset == "only_local":
            G_t = nx.DiGraph()
        else:
            G_t = nx.complete_graph(nodes, nx.DiGraph())
            for e in G_t.edges():
                G_t[e[0]][e[1]]['weight'] = 1

        # Iterate over all nodes in the list and add a self-loop
        for node in nodes:
            G_t.add_edge(node, node, weight=1)
        
        for date in dates:
            graph_list.append(G_t)
            # print(G_t)

    return graph_list






def generate_new_features(Gs, labels, dates, window=7, scaled=False, lockdown_indicator=None, lockdown=None):
    """
    Generate node features
    Features[1] contains the features corresponding to y[1]
    e.g. if window = 7, features[7]= day0:day6, y[7] = day7
    if the window reaches before 0, everything is 0, so features[3] = [0,0,0,0,day0,day1,day2], y[3] = day3
    """
    features = list()
    
    labs = labels.copy()
    if lockdown:
        lock = lockdown_indicator.copy()
  

    # TODO Gs are the graphs for each timestep
    for idx,G in enumerate(Gs):
        #  Features = population, coordinates, d past cases, one hot region
        # TODO population could be added here
        if lockdown:
            H = np.zeros([G.number_of_nodes(),window+1]) #+3+n_departments])#])#])
        else:
            H = np.zeros([G.number_of_nodes(),window]) #+3+n_departments])#])#])
        me = labs.loc[:, dates[:(idx)]].mean(1)
        sd = labs.loc[:, dates[:(idx)]].std(1)+1

        ### enumarate because H[i] and labs[node] are not aligned
        # TODO: i think i need the nodelist here again to get features in same order
        for i,node in enumerate(G.nodes()):
            #---- Past cases      
            if(idx < window):# idx-1 goes before the start of the labels
                if(scaled):
                    H[i,(window-idx):(window)] = (labs.loc[node, dates[0:(idx)]] - me[node])/ sd[node]
                else:
                    H[i,(window-idx):(window)] = labs.loc[node, dates[0:(idx)]]

            elif idx >= window:
                if(scaled):
                    H[i,0:(window)] =  (labs.loc[node, dates[(idx-window):(idx)]] - me[node])/ sd[node]
                else:
                    H[i,0:(window)] = labs.loc[node, dates[(idx-window):(idx)]]

            # Population would be window + 1
            # pops.loc[node/i,0] <- county, population in file
            if lockdown:
                if idx-1 >= 0:
                    H[i, window] = lock.loc[node, dates[idx-1]] # need idx-1 because idx is the dependent day
                else:
                    H[i, window] = lock.loc[node, dates[idx]]
        # List of features for the window days before the index
        features.append(H)
        
    return features






def generate_new_batches(Gs, features, y, idx, graph_window, shift, batch_size, device, test_sample):
    """
    Generate batches for graphs for MPNN
    """

    N = len(idx)
    n_nodes = Gs[0].shape[0]
    #n_nodes = Gs[0].number_of_nodes()
  
    adj_lst = list()
    features_lst = list()
    y_lst = list()

    for i in range(0, N, batch_size):
        n_nodes_batch = (min(i+batch_size, N)-i)*graph_window*n_nodes
        # print(n_nodes_batch)
        step = n_nodes*graph_window

        adj_tmp = list()
        features_tmp = np.zeros((n_nodes_batch, features[0].shape[1]))

        y_tmp = np.zeros((min(i+batch_size, N)-i)*n_nodes)

        #fill the input for each batch
        for e1,j in enumerate(range(i, min(i+batch_size, N) )):
            val = idx[j]

            # Feature[10] containes the previous 7 cases of y[10] -> meaning 9, 8, 7, 6, 5, 4, 3
            for e2,k in enumerate(range(val-graph_window+1,val+1)):
                
                # need the graph from the previous day, as k contains previous features
                adj_tmp.append(Gs[k-1].T)  
                # each feature has a size of n_nodes
                features_tmp[(e1*step+e2*n_nodes):(e1*step+(e2+1)*n_nodes),:] = features[k] #-features[val-graph_window-1]
                 
            
            if(test_sample>0):
                #--- val is by construction less than test sample
                if(val+shift<test_sample):
                    y_tmp[(n_nodes*e1):(n_nodes*(e1+1))] = y[val+shift]
                    
                else:
                    y_tmp[(n_nodes*e1):(n_nodes*(e1+1))] = y[val]
                        
                        
            else:
                y_tmp[(n_nodes*e1):(n_nodes*(e1+1))] = y[val+shift]
        
        adj_tmp = sp.block_diag(adj_tmp)
        adj_lst.append(sparse_mx_to_torch_sparse_tensor(adj_tmp).to(device))
        features_lst.append(torch.FloatTensor(features_tmp).to(device))
        y_lst.append(torch.FloatTensor(y_tmp).to(device))

    return adj_lst, features_lst, y_lst






def generate_batches_lstm(n_nodes, y, idx, window, shift, batch_size, device,test_sample):
    """
    Generate batches for graphs for the LSTM
    """
    N = len(idx)
    features_lst = list()
    y_lst = list()
    adj_fake = list()
    
    for i in range(0, N, batch_size):
        n_nodes_batch = (min(i+batch_size, N)-i)*n_nodes*1
        #step = n_nodes#*window
        step = n_nodes*1

        adj_tmp = list()
        features_tmp = np.zeros((window, n_nodes_batch))
        
        y_tmp = np.zeros((min(i+batch_size, N)-i)*n_nodes)
        
        for e1,j in enumerate(range(i, min(i+batch_size, N))):
            val = idx[j]
            
            # keep the past information from val-window until val-1
            for e2,k in enumerate(range(val-window,val)):
               
                if(k==0): 
                    features_tmp[e2, (e1*step):(e1*step+n_nodes)] = np.zeros([n_nodes])
                else:
                    features_tmp[e2, (e1*step):(e1*step+n_nodes)] = np.array(y[k])

            if(test_sample>0):
                if(val+shift<test_sample):
                    y_tmp[(n_nodes*e1):(n_nodes*(e1+1))] = y[val+shift]
                else:
                    y_tmp[(n_nodes*e1):(n_nodes*(e1+1))] = y[val]
                        
            else:
         
                y_tmp[(n_nodes*e1):(n_nodes*(e1+1))] = y[val+shift]       
         
        adj_fake.append(0)
        
        features_lst.append(torch.FloatTensor(features_tmp).to(device))
        y_lst.append( torch.FloatTensor(y_tmp).to(device))
        
    return adj_fake, features_lst, y_lst




def sparse_mx_to_torch_sparse_tensor(sparse_mx):
    """Convert a scipy sparse matrix to a torch sparse tensor."""
    sparse_mx = sparse_mx.tocoo().astype(np.float32)
    indices = torch.from_numpy(
        np.vstack((sparse_mx.row, sparse_mx.col)).astype(np.int64))
    values = torch.from_numpy(sparse_mx.data)
    shape = torch.Size(sparse_mx.shape)
    return torch.sparse.FloatTensor(indices, values, shape)




class AverageMeter(object):
    """Computes and stores the average and current value"""
    def __init__(self):
        self.reset()

    def reset(self):
        self.val = 0
        self.avg = 0
        self.sum = 0
        self.count = 0

    def update(self, val, n=1):
        self.val = val
        self.sum += val * n
        self.count += n
        self.avg = self.sum / self.count
        

