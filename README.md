# TTH

Using this package the user can evaluate the tree level contracted with tree-level amplitude, bare one-loop amplitude, and UV counterterms for $gg\to t\overline{t}H$ for any given phase space point $\{s_{12}, s_{13}, s_{14}, s_{23}, s_{24}, s_{34}, m_t^2\}$. Computation details can be found [here](https://arxiv.org/abs/2312.10015)

## Installation

```bash
git clone git@github.com:p-a-kreer/TTH.git
```

Change to the package directory and unzip the input_files

```bash
cd TTH/ && unzip input_files.zip
```

You can check the installation by running all cells in the mathematica file **tutorial.wl** or using

```bash 
math -script tutorial.wl
```

in the terminal.

## Usage

The package provides three functions: 

1. TTHAmplitudeTreeTree $=\mathcal{N}\text{Re}\left[\overline{\sum}(\mathcal{A}^{(0)})^{\dagger}\mathcal{A}^{(0)}\right]$
2. TTHAmplitudeLoopTree $=2\mathcal{N}\text{Re}\left[\overline{\sum}(\mathcal{A}^{(0)})^{\dagger}\mathcal{A}^{(1)}\right]$
3. TTHUVCounter $=2\mathcal{N}\text{Re}\left[\overline{\sum}(\mathcal{A}^{(0)})^{\dagger}\mathcal{A}_{\rm ct.}\right]$

where $\mathcal{N}=4\pi \alpha_s^3 y_t^2$ is a normalization constant. The input is a mathematica rule for the kinematics of the form  

```mathematica
{s12 -> 1000000, s13 -> -(15393705013/47152), 
 s14 -> -(39849685741/932940), s23 -> -(21485226445/77264), 
 s24 -> -(48342263815/112029), s34 -> 83218910153/383674, 
 mt2 -> 526133493760001/17179869184}
```

The default values for 

1. Number of massless quarks **NF**
2. Number of colors  **NC**
3. Yukawa coupling **yt**
4. Strong coupling constant $\[Alpha]S$
5. Precision **PrecisionGoal**
6. Supressing intermediate print statements **SilentMode**

can be changed by adapting the command 

```mathematica
THOptions["NF" -> 5, "NC" -> 3, "yt" -> 82979727/120018599, "\[Alpha]S" -> 59/500, "PrecisionGoal" -> 6, "SilentMode" -> False]
```

## Evaluation of Helicity Form Factors

It is possible to evaluate the unrenormalized helicity formfactors $\mathcal{F}$ with the function NHelicityFormFactors. Similar to the previous functions this function takes a list of kinematic replacement rules and returns the helicity formfactors. Note that, the helicity form factors are computed for rescaled kinematics (see Sec. 2), where $m_t=1$. 

The output is a vector of $8$ entries, where each entry corresponds to one helicity form factor. In addition to the quantities above, there are four additional variables

1. TATB, TBTA, DeltaAB = $|\mathcal{C}_1\rangle, |\mathcal{C}_2\rangle, |\mathcal{C}_3\rangle$
2. tr5 = $tr_5$ 





For questions and comments check out or comment [here](https://alphaxiv.org/abs/2312.10015) and do not hesitate to contact the authors: 
Federico Buccioni, Philipp Alexander Kreer, Xiao Liu, and Lorenzo Tancredi


