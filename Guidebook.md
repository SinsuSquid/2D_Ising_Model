# A R1215's Guide to Monte-Carlo Simulation
![Monte-Carlo](https://upload.wikimedia.org/wikipedia/commons/thumb/2/2f/Monaco_Monte_Carlo_1.jpg/640px-Monaco_Monte_Carlo_1.jpg)
<br>Monte Carlo [_"MON-tee KAR-loh"_], Monaco<br>(Source : Wikipedia)

## Fundamental Statistical Mechanics
- Deterministric <br> : "if position($r(t_0)$) and momentum($p(t_0)$) are determined, $r(t)$ and $p(t)$ also can be determined w.r.t. any value of t" <br>> initial configuration ($r(t_0)$ & $p(t_0)$) are _super_ important ! 
    - $\vec{F} = m\vec{a}=\frac{d\vec{p}}{dt}$ can be used to calculate a new $r(t)$ and $p(t)$ ! 

- Phase Space
![Phase Space Representation](https://upload.wikimedia.org/wikipedia/commons/e/ea/Simple_Harmonic_Motion_Orbit.gif)
<br>(Source : Wikipedia)


- Time Average of the observable A : <br>$\braket{A} = \lim_{t \to \infty}\frac{1}{\tau}\int^{\tau}_{0}A(\textbf{p}^N(t)\ \textbf{r}^N(t))\ dt$
<br>> It's simple, but calculating $r(t)$ & $p(t)$ for evert t for N particles is __IMPOSSIBLE__.

- Ensembles
<br>: a collection of a very large number of systems of interest
<br>> Why don't we generate __LOTS LOTS LOTS__ of $r(t_0)$ and $p(t_0)$ and do something with it?
    - Types of Ensembles
        - ![Types of Ensembles](https://upload.wikimedia.org/wikipedia/commons/5/5e/Statistical_Ensembles.png)
        <br>(Source : Wikipedia)
        - Microcanonical (NVE)
        - Canonical (NVT)
        - Isothermal-Isobaric (NpT)
        - Grand-Canonical ($\mu$ VT)

- Partition Function, Q
    - For Canonical Ensemble (NVT),
    <br>$P(\textbf{r}^N\ \textbf{p}^N) = \frac{1}{Q}\ e^{-H(\textbf{r}^N\ \textbf{p}^N)/k_BT}$ (Probability)
    <br>with $Q = \int\int d\textbf{p}^N d\textbf{r}^N e^{-H(\textbf{r}^N\ \textbf{p}^N)/k_BT}$ (Partition Function)
    - $\textbf{r}^N$ : the coordinates of all N particles
    - $\textbf{p}^N$ : the momenta of all N particles
    - $H(\textbf{r}^N\ \textbf{p}^N)$ : Hamiltonian (energy) of the system
        - $H(\textbf{r}^N\ \textbf{p}^N)$ = $K(\textbf{r}^N\ \textbf{p}^N)$ + $U(\textbf{r}^N\ \textbf{p}^N)$
        - $K$ : the kinetic energy of the system -> $\textbf{p}^N$
        - $U$ : the potential energy of the system -> $\textbf{r}^N$

- Ensemble Average of the observable A : <br>$\braket{A} = \frac{\int\int d\textbf{p}^N d\textbf{r}^N A(\textbf{p}^N,\ \textbf{r}^N)\ e^{-\beta H(\textbf{r}^N\ \textbf{p}^N)}}{\int\int d\textbf{p}^N d\textbf{r}^Ne^{-\beta H(\textbf{r}^N\ \textbf{p}^N)}}$, with $\beta = \frac{1}{k_BT}$

- ![Ensemble](https://upload.wikimedia.org/wikipedia/commons/f/f7/Hamiltonian_flow_classical.gif)
<br>(Source : Wikipedia)


- Ergodic "Postulate" : "Time Average" == "Ensemble Average"
<br>> We use this postulate for systems in equilibrium state.


## The Monte-Carlo Method
- Ensemble Average of the observable A : <br>$\braket{A} = \frac{\int\int d\textbf{p}^N d\textbf{r}^N A(\textbf{p}^N,\ \textbf{r}^N)\ e^{-\beta H(\textbf{r}^N\ \textbf{p}^N)}}{\int\int d\textbf{p}^N d\textbf{r}^Ne^{-\beta H(\textbf{r}^N\ \textbf{p}^N)}}$, with $\beta = \frac{1}{k_BT}$
    - $A(\textbf{p}^N)$ can be carried out analytically since it's quadratic.
    - What about $A(\textbf{r}^N)$ ?
        - Do we really have to search for $\int d\textbf{r}^N A(\textbf{r}^N)\ e^{-\beta U(\textbf{r}^N)}$ ?
        - So many integrations!!
        - Since Boltzmann Factor decays quickly, it's mostly zero.

- Importance Sampling
    - We sample many points in the region where the Boltzmann factor is large and few elsewhere. <br>(Uniniformly distrubuted sampling points)
    - Here's how
    <br>[An introduction to importance sampling](https://www.youtube.com/watch?v=V8f8ueBc9sY)
    <br>(Thanks, YouTube !)

## Metropolis Method


