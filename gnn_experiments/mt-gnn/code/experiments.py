#!/usr/bin/env python
# coding: utf-8

import os
import time
import argparse
import numpy as np


import torch
import torch.nn.functional as F
import torch.optim as optim

from math import ceil

import pandas as pd


from utils import generate_new_batches, AverageMeter, read_meta_datasets
from models import FEED_FORWARD, MPNN_LSTM2, MPNN_LSTM, MPNN_LSTM_no_skip, MPNN, MPNN_add
        

    
def train(adj, features, y):
    optimizer.zero_grad()
    output = model(adj, features)
    loss_train = F.mse_loss(output, y)
    loss_train.backward(retain_graph=True)
    optimizer.step()
    return output, loss_train



def test(adj, features, y):    
    output = model(adj, features)
    loss_test = F.mse_loss(output, y)
    return output, loss_test



if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('--epochs', type=int, default=500,
                        help='Number of epochs to train.')
    parser.add_argument('--lr', type=float, default=0.001,
                        help='Initial learning rate.')
    parser.add_argument('--hidden', type=int, default=64,
                        help='Number of hidden units.')
    parser.add_argument('--batch-size', type=int, default=8,
                        help='Size of batch.')
    parser.add_argument('--dropout', type=float, default=0.5,
                        help='Dropout rate.')
    parser.add_argument('--window', type=int, default=7,
                        help='Size of window for features.')
    parser.add_argument('--graph-window', type=int, default=7,
                        help='Size of window for graphs in MPNN LSTM.')
    parser.add_argument('--recur',  default=False,
                        help='True or False.')
    parser.add_argument('--early-stop', type=int, default=100,
                        help='How many epochs to wait before stopping.')
    parser.add_argument('--start-exp', type=int, default=15,
                        help='The first day to start the predictions.')
    parser.add_argument('--startahead', type=int, default=0,
                        help='The number of days ahead of the train set the predictions should reach.')
    parser.add_argument('--ahead', type=int, default=14,
                        help='The number of days ahead of the train set the predictions should reach.')
    parser.add_argument('--sep', type=int, default=10,
                        help='Seperator for validation and train set.')
    parser.add_argument('--outshift', type=str, default='../results',)
    parser.add_argument('--outtest', type=str, default='../output',)
    parser.add_argument('--checkpoints', type=str, default='../checkpoints',)
    parser.add_argument('--dataset', type=str, default='normal',choices=["normal", "binary", "distance", "only_local", "full", "scaled_amount", "scaled_structure"])
    parser.add_argument('--model', type=str, default='mpnn_lstm',choices=["mpnn", "mpnn_add", "mpnn_lstm", "mpnn_lstm2", "feed_forward", "mpnn_lstm_no_skip"])
    parser.add_argument('--lockdown', type=str, default=None , choices=[None, "boolean", "kappa"])
    parser.add_argument('--cases', type=str, default="new_cases_norm" , choices=["new_cases", "active_cases", "new_cases_norm"])
    
    
    args = parser.parse_args()
    device = torch.device("cuda" if torch.cuda.is_available() else torch.device("cpu"))
    print(f"Device: {device}")
    
    
    meta_labs, meta_graphs, meta_features, meta_y = read_meta_datasets(args.window, args.dataset, args.lockdown, args.cases)
    
    # TODO this loop can be completely removed
    for country in ["GER"]: 
        if(country=="GER"):
            idx = 0

        print(f"Country: {country}")
            
        labels = meta_labs[idx]
        gs_adj = meta_graphs[idx]
        features = meta_features[idx]
        y = meta_y[idx]
        n_samples= len(gs_adj)
        nfeat = meta_features[0][0].shape[1]

        
        n_nodes = gs_adj[0].shape[0]
        print(f"Number of nodes: {n_nodes}")
        if not os.path.exists(args.outshift):
            os.makedirs(args.outshift)
        fw = open(args.outshift+"/results_"+args.model+"_"+country+"_"+args.dataset+".csv","a")
        # Files will then need to be appended, but as I'm using different runs this is the easiest way 

        
        for args.model in [args.model]:
            
			#---- predict days ahead , 0-> next day etc.
            for shift in list(range(args.startahead,args.ahead)):
                print(f"Shift: {shift}")
                result = []
                exp = 0

                for test_sample in range(args.start_exp,n_samples-shift):#
                    exp+=1
                    print(f"Test sample: {test_sample}")

                    #----------------- Define the split of the data
                    # this needs to be in a way, that the last sample used has test_sample - 1 as target
                    # --> idx + shift == test_sample-1
                    idx_train = list(range(args.window-1, test_sample-shift-args.sep))
                    
                    # TODO this way the validation set becomes more or less useless for higher shifts, right?
                    idx_val = list(range(test_sample-shift-args.sep,test_sample-shift,2))
                    print(f"VAL: {idx_val}") 
                                     
                    idx_train = idx_train+list(range(test_sample-shift-args.sep+1,test_sample-shift,2))
                    print(f"TRAIN: {idx_train}")


                    if(args.model in ["mpnn_lstm", "mpnn_lstm2", "mpnn_lstm_no_skip"]):
                        adj_train, features_train, y_train = generate_new_batches(gs_adj, features, y, idx_train, args.graph_window, shift, args.batch_size,device,test_sample)
                        adj_val, features_val, y_val = generate_new_batches(gs_adj, features, y, idx_val, args.graph_window,  shift,args.batch_size, device,test_sample)
                        # TODO I think train_sample should be -1 here as well, like mpnn -> otherwise I have the same target for all shifts
                        adj_test, features_test, y_test = generate_new_batches(gs_adj, features, y,  [test_sample], args.graph_window,shift, args.batch_size, device,-1)

                    else:
                        adj_train, features_train, y_train = generate_new_batches(gs_adj, features, y, idx_train, 1,  shift,args.batch_size,device,test_sample)
                        adj_val, features_val, y_val = generate_new_batches(gs_adj, features, y, idx_val, 1,  shift,args.batch_size,device,test_sample)
                        adj_test, features_test, y_test = generate_new_batches(gs_adj, features, y,  [test_sample], 1,  shift,args.batch_size, device,-1)
                        # print(len(features_test))


                    n_train_batches = ceil(len(idx_train)/args.batch_size)
                    n_val_batches = 1
                    n_test_batches = 1


                    #-------------------- Training
                    # Model and optimizer
                    stop = False#
                    while(not stop):#

                        if(args.model=="mpnn_lstm"):

                            model = MPNN_LSTM(nfeat=nfeat, nhid=args.hidden, nout=1, n_nodes=n_nodes, window=args.graph_window, dropout=args.dropout).to(device)
                            model_parameters = filter(lambda p: p.requires_grad, model.parameters())
                            print(f"PARAMETERS: {sum([np.prod(p.size()) for p in model_parameters])}")

                        elif(args.model=="mpnn_lstm2"):

                            model = MPNN_LSTM2(nfeat=nfeat, nhid=args.hidden, nout=1, n_nodes=n_nodes, window=args.graph_window, dropout=args.dropout).to(device)
                            model_parameters = filter(lambda p: p.requires_grad, model.parameters())
                            print(f"PARAMETERS: {sum([np.prod(p.size()) for p in model_parameters])}")

                        elif(args.model=="mpnn_lstm_no_skip"):

                            model = MPNN_LSTM_no_skip(nfeat=nfeat, nhid=args.hidden, nout=1, n_nodes=n_nodes, window=args.graph_window, dropout=args.dropout).to(device)
                            model_parameters = filter(lambda p: p.requires_grad, model.parameters())
                            print(f"PARAMETERS: {sum([np.prod(p.size()) for p in model_parameters])}")

                        elif(args.model=="feed_forward"):

                            model = FEED_FORWARD(nfeat=nfeat, nhid=args.hidden, nout=1, n_nodes=n_nodes, window=args.graph_window, dropout=args.dropout).to(device)
                            model_parameters = filter(lambda p: p.requires_grad, model.parameters())
                            print(f"PARAMETERS: {sum([np.prod(p.size()) for p in model_parameters])}")

                        elif(args.model=="mpnn"):
                            model = MPNN(nfeat=nfeat, nhid=args.hidden, nout=1, dropout=args.dropout).to(device)
                        elif(args.model=="mpnn_add"):
                            model = MPNN_add(nfeat=nfeat, nhid=args.hidden, nout=1, dropout=args.dropout).to(device)

                        optimizer = optim.Adam(model.parameters(), lr=args.lr)
                        scheduler = torch.optim.lr_scheduler.ReduceLROnPlateau(optimizer, patience=10)

                        #------------------- Train
                        best_val_acc= 1e8
                        val_among_epochs = []
                        train_among_epochs = []
                        stop = False

                        for epoch in range(args.epochs):   
                            # print(epoch) 
                            start = time.time()

                            model.train()
                            train_loss = AverageMeter()

                            # Train for one epoch
                            for batch in range(n_train_batches):
                                output, loss = train(adj_train[batch], features_train[batch], y_train[batch])
                                train_loss.update(loss.data.item(), output.size(0))
                            # Evaluate on validation set
                            model.eval()

                            output, val_loss = test(adj_val[0], features_val[0], y_val[0])
                            val_loss = float(val_loss.detach().cpu().numpy())


                            # Print results
                            if(epoch%50==0):
                                print("Epoch:", '%03d' % (epoch + 1), "train_loss=", "{:.5f}".format(train_loss.avg),"val_loss=", "{:.5f}".format(val_loss), "time=", "{:.5f}".format(time.time() - start))

                            train_among_epochs.append(train_loss.avg)
                            val_among_epochs.append(val_loss)

                            # removed stuck protection
                            # if(epoch<30 and epoch>10):
                            #     if(len(set([round(val_e) for val_e in val_among_epochs[-20:]])) == 1 ):
                            #         #stuck= True
                            #         stop = False
                            #         break
              
                            if( epoch>args.early_stop):
                                if(len(set([round(val_e) for val_e in val_among_epochs[-50:]])) == 1):#
                                    print("break")
                                    #stop = True
                                    break

                            stop = True


                            #--------- Remember best accuracy and save checkpoint
                            # TODO maybe I should add the shift here? as they are different models
                            if val_loss < best_val_acc:
                                # print(f"Best val loss: {val_loss}")
                                best_val_acc = val_loss
                                torch.save({
                                    'state_dict': model.state_dict(),
                                    'optimizer' : optimizer.state_dict(),
                                }, f'{args.checkpoints}/{args.dataset}_{args.model}_{shift}_{test_sample}_model_best.pth.tar')

                            scheduler.step(val_loss)

                    np.savetxt(args.outtest+"/tain_loss_"+args.dataset+"_"+args.model+"_"+country+"_"+str(test_sample)+"_"+str(shift)+".csv", train_among_epochs, delimiter = ',')
                    np.savetxt(args.outtest+"/val_loss_"+args.dataset+"_"+args.model+"_"+country+"_"+str(test_sample)+"_"+str(shift)+".csv", val_among_epochs, delimiter = ',')
                    print("validation")  
 
                    #---------------- Testing
                    test_loss = AverageMeter()

                    #print("Loading checkpoint!")
                    checkpoint = torch.load(f'{args.checkpoints}/{args.dataset}_{args.model}_{shift}_{test_sample}_model_best.pth.tar')
                    model.load_state_dict(checkpoint['state_dict'])
                    optimizer.load_state_dict(checkpoint['optimizer'])
                    model.eval()

                    output, loss = test(adj_test[0], features_test[0], y_test[0])


                    if(args.model=="LSTM"):
                        o = output.view(-1).cpu().detach().numpy()
                        l = y_test[0].view(-1).cpu().numpy()
                    else:
                        o = output.cpu().detach().numpy()
                        l = y_test[0].cpu().numpy()

	                # average error per county
                    # Error is per shift+test_sample -> maybe save that as well -> look at how the error changes over time (maybe specific times can't be well predicted)
                    error = np.sum(abs(o-l))/n_nodes

                    df = pd.DataFrame({'county': labels.index,'prediction':o, 'observed':l})
                    if not os.path.exists(args.outtest):
                        os.makedirs(args.outtest)
                    # Should be able to verify this by looking at test_sample and check real values of days after shift
                    df.to_csv(args.outtest+"/out_"+args.dataset+"_"+args.model+"_"+country+"_"+str(test_sample)+"_"+str(shift)+".csv",index=False) 
                    

                    # Print results
                    print("test error=", "{:.5f}".format(error))
                    result.append(error)

                print("{:.5f}".format(np.mean(result))+",{:.5f}".format(np.std(result))+",{:.5f}".format(np.sum(labels.iloc[:,args.start_exp:test_sample].mean(1))))

                # TODO this doesn't seem to work replace with own solution from mt-egcn
                fw.write(str(args.model)+","+str(shift)+",{:.5f}".format(np.mean(result))+",{:.5f}".format(np.std(result))+"\n")

    fw.close()



