# RRcourse-project
Repository for the Reproducible research classes 2022. Improvements regarding reproducibility of bachelor thesis about classification of pulsar stars. 


# Renv instructions:

To reproduce the working environment simply run `renv::restore()` in the directory
with the project. In some cases you may also need to activate the project first. 
If you see the following after calling `renv::restore()`

*This project has not yet been activated.
Activating this project will ensure the project library is used during restore.
Please see `?renv::activate` for more details.

Would you like to activate this project before restore? [Y/n]*

Just accept by typing *Y* and pressing enter. 

Then if you update or install any package that will be needed in the project just run 
`renv::snapshot()` and renv lockfile will be updated. Then just push your changes 
to the remote repository. 
