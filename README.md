Kalman_photons is a kalman filter algorithm for estimating prior probabilities of a two hidden homogeneous markov chains of two sources of photons.
Sources can be active or inactive, when active they emit photons, when inactive they dont.
The photon fluxes emitted by the source and during the period of its activity are governed by Poisson's difference with parameter λ_i

The R file is a solution to problem:

Consider two sources of photons. In each discrete case t = 0, 1, 2, . . . each source is either active
(emits photons) or inactive (does not emit photons). In cases t = 1, 2, . . . . . the detector can measure the total
the total number of photons emitted together by the two sources over a constant time interval
lengths. For simplicity, let us assume that the detector captures all photons emitted by the sources.
For t = 0, 1, 2, . . . . and i = 1, 2, let us define a random quantity Zt,i, where Zt,i = 0 if source i is not in the case t
active and Zt,i = 1 if source i is active in case t. The sequences (Z0,i, Z1,i, . . .) form an independent homogeneous
Markovian cross sections. If the source is also inactive in case t, it does not emit photons. If the source is also active, it emits
photons. The number of photons emitted by the source and during the period of its activity is given by Poisson's
difference with parameter λi
. Let us denote the number of photons emitted by source i at time t as Xt,i and the number of
of photons captured by the detector at time t is denoted by Yt := Xt,1 + Xt,2.

Assume that the a priori difference between the two states
f_{Z0,1,Z0,2}
transition probablities
f_{Zt+1,1|Zt,1}, {f_Zt+1,2|Zt,2}
and the intensities λ1, λ2 are known.
Derive recursive relations for the so-called state filtering, i.e. relations for conditional truth functions
f_{Zt,1,Zt,2|Y1,Y2,...,Yt}
of the state in case t under the conditions of all currently available observations
