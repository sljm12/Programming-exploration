These are my experiments in using the US election data to train models to detect who is speaking i.e stylometry

I use the transcripts from the various speeches and debates for training my models.

These are the following approaches that i have tried

- Simple Dense models -> Not very good can't seem to get more that 70% accuracy after 300 epochs
- Bi directional LSTM -> After 30 epochs can get about 89% accuracy
