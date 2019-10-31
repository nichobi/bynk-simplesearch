# SimpleSearch

SimpleSearch is a simple file searcher that takes a directory of text files as input, and then lets you perform searches on their contents. A search can be any number of words. SimpleSearch considers words to be a nonzero-length series of letters, separated by spaces. All searches are case-insensitive.

To run SimpleSearch, launch `sbt` in the project directory and enter `run path/to/directory` where `path/to/directory` is the directory containing the text files you want to search through. There are some example files in `resources/test` that you can try searching throgh.

The results will be presented as a list of files and the percentage of the search words they contain. Results are sorted by this percentage and will be limited to at most the ten best matching files.

To exit SimpleSearch type `:quit` or `:q`
