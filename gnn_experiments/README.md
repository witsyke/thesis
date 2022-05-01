# Run GNN experiments
Take not that the mobility data is not publicly available.
However experiments with only_local (with the identity matrix as adjacency) data can be run.

1. Navigate to the mt-gnn/code directory
2. Run the following command for the default experiment (mpnn_lstm, observed mobility, normalized new cases)
```bash
experiments.py
```
3. For a list of available options run:
 ```bash
experiments.py --help 
```  