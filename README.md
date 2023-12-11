# TTH

Using this package the user can evaluate the tree level contracted with tree-level amplitude, bare one-loop amplitude, and UV counterterms for $gg\to t\overline{t}H$ for any given phase space point $\{s_{12}, s_{13}, s_{14}, s_{23}, s_{24}, s_{34}, m_t^2\}$

## Instalation

```bash
git clone git@github.com:p-a-kreer/TTH.git
```

change in the package directory

```bash
cd TTH/
```

and unpack the input files

```bash
unzip input_files.zip
```

You can check the installation by running all cells in the mathematica file **tutorial.wl**

## Usage

The package provides three functions: 

1. TTHAmplitudeTreeTree $=\mathcal{N}\text{Re}\left[\overline{\sum}(\mathcal{A}^{(0)})^{\dagger}\mathcal{A}^{(0)}\right]$
2. TTHAmplitudeLoopTree $=2\mathcal{N}\text{Re}\left[\overline{\sum}(\mathcal{A}^{(0)})^{\dagger}\mathcal{A}^{(1)}\right]$
3. TTHUVCounter $=2\mathcal{N}\text{Re}\left[\overline{\sum}(\mathcal{A}^{(0)})^{\dagger}\mathcal{A}_{\rm ct.}\right]$


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





