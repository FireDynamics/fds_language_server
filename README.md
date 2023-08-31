# FDS Language Server

## About
This is the server side for a fds language extension.

## Building
To Build this project [rust](https://www.rust-lang.org/) hast to be installed.  

Linux:
```bash
git clone https://github.com/FireDynamics/fds_language_server
cd fds_language_server
cargo build --release
```

## Extensions
To use this language server simply install the client extension. All available extensions are listed below:
- [VS Code](https://github.com/FireDynamics/vs_code_fds_language_client)

## Current Features
- [x] Autocomplete for class and property names
- [X] Autocomplete for IDs (Only works for Fields with "`_ID`" as an ending)
- [x] Hover information for classes and properties
- [x] Syntax highlighting
- [x] Syntax error messages
- [x] Display how many cells the the whole simulation has.
- [X] Display the size, cell size and cell count a mesh has.
- [x] Underlying support for different versions
    - Place the `fds_classes.csv` and `fds_defaults.csv` file under `fds/data/` inside a respecting folder with the version number to enable a different version.
    - Start the FDS file with `Version 6.1.4` (change version number)
- [x] Auto formrating.