# v3.3 GitHub publication-ready release

Target repository: https://github.com/srudkin12/Red-Wall

Changes from v3.2:

- records the final GitHub repository URL in `README.md`, `CITATION.cff` and reproducibility documentation;
- adds `PUBLISH_TO_GITHUB.md`;
- adds conservative `scripts/publish_to_github.sh`;
- uses the SSH remote `git@github.com:srudkin12/Red-Wall.git`;
- does not store or request a private SSH key;
- refuses to force-overwrite any existing remote branch history;
- retains the no-empirical-data policy and Paweł Dłotko attribution/citation requirement for `framework/BallMapper.cpp`.

Software licence remains `NOASSERTION`; no blanket licence has been inferred for third-party code.
