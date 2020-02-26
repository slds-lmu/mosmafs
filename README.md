
# MOSMaFS: Multi-Objective Simultaneous Model and Feature Selection

[![Travis build status](https://travis-ci.com/compstat-lmu/mosmafs.svg?branch=mosmafs-package)](https://travis-ci.com/compstat-lmu/mosmafs)
[![Coverage](https://codecov.io/github/compstat-lmu/mosmafs/branch/mosmafs-package/graphs/badge.svg)](https://codecov.io/github/compstat-lmu/mosmafs)
[![CRAN Status Badge](https://www.r-pkg.org/badges/version/mosmafs)](https://CRAN.R-project.org/package=mosmafs)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/mosmafs)](https://CRAN.R-project.org/package=mosmafs)

## Project Status

We are on CRAN! You can also read [our paper on arxiv](https://arxiv.org/abs/1912.12912).

## Installation

It is best to install the official version from CRAN. If you want to live on the edge, you can also install the github version, using `remotes`:

```r
remotes::install_github("compstat-lmu/mosmafs", ref = "mosmafs-package")
```

## Documentation

* [Introduction](https://compstat-lmu.github.io/mosmafs/articles/demo.html)
* [Multi-Fidelity](https://compstat-lmu.github.io/mosmafs/articles/multifidelity.html)

## Citation

If `mosmafs` is useful for your research, please cite our paper:

*Binder, Martin., Moosbauer, J. et al. "Model-Agnostic Approaches to Multi-Objective Simultaneous Hyperparameter Tuning and Feature Selection." arXiv preprint arXiv:1912.12912 (2019).*

BibTex:
```
@misc{binder2019multiobjective,
    title={Multi-Objective Hyperparameter Tuning and Feature Selection using Filter Ensembles},
    author={Martin Binder and Julia Moosbauer and Janek Thomas and Bernd Bischl},
    year={2019},
    eprint={1912.12912},
    archivePrefix={arXiv},
    primaryClass={stat.ML}
}
```

## License

MIT License
