REDM
====

A collection of R packages for educational datamining

# Bayesian Knowledge Tracing

## Examples

### Simulate learning performance from given parameters

```R
# create a parameters object. In order: (init, learn, guess, slip)
p <- as.bkt.params( c( 0.1, 0.15, 0.2, 0.25) )

# simulate up to 20 learning steps for 10 students:
sim <- bkt.sim( 10, 20, p )
```

### Use brute-force search to recover parameters

```R
bforce.search( sim$opps )

# search over finer grained grid
# CAUTION: this can become *very* slow
bforce.search( sim$opps, bforce.search.grid(0.01) )
```

### Recover parameters via Hidden Markov Model inference

```R
# fit a BKT HMM with random initial params and simulated data
hmm <- bkt.hmm.fit( as.bkt.params( runif(4) ), sim$opps )
hmm
```