# Database Schema

The database schema is currently stored in this directory as `${Model Name}.persistentmodels` files.
To add a new model, create a new file and create a corresponding file in `src/GT/DB/${Model Name}.hs` that loads the file.
You can use any of the other files as a template.

Types that you reference in this must be available in the Haskell file - so if you need to import something for your type to work, it'll need to be imported in that file too.
You'll also need to ensure that the [Migration](../src/GT/DB/Migration.hs) module has the same imports.
