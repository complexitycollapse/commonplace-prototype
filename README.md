# Commonplace Prototype

Commonplace is an attempt to implement Xanadu, the original hypertext system conceived by Ted Nelson.

This is not Commonplace, but a prototype for part of an eventual system. It implements one part of a Xanadu-like system: the creation and local management of xanadocs (the Xanadu equivalent of an HTML page). A minimal Xanadu system would also require a server to serve the xanadocs (the equivalent of a web server) and a "browser" to act as a user interface.

This prototype supports the following operations:

- Initializing a new "repository" (i.e. a local xanadoc store in which new xanadocs can be created).
- Creating and editing xanadocs.
- Importing/exporting text to/from xanadocs.
- Transclusion (i.e. the sharing of content between two or more xanadocs).
- Adding links to xanadocs.

Although it is not supported by the API, internally the system also implements a "publishing" algorithm, which converts a xanadoc and its raw contents into the form required for publishing to the outside world. This is the point where all the permanent, immutable names are created for all the content you wish to publish. (Note that the mechanism used to achieve this is novel, but is inspired by the "permascroll" concept used in some official Xanadu implementations).

Note that this system has several limitations, such as

- It is only a crude CLI tool. There is no rendering, browsing or WYSIWYG creation of xanadocs.
- There is no server. Files are stored and retrieved locally.
- It supports only ASCII text as document contents.

However, it successfully demonstrates a potential means of storing, editing and publishing xanadocs.

## Note on Terminology

Xanadu is the trade name used by Ted Nelson for his various hypertext systems. Hypertext and hypermedia were the original generic names that Nelson created for the concept he was attempting to implement. However, today the words hypertext and hypermedia have very different meanings, due to the influence of pseudo-hypertext systems such as the World Wide Web, Hypercard and others. Therefore Nelson coined the term "transliterature" as a replacement for hypertext/hypermedia to describe his original concept. I have described Commonplace as a "Xanadu-like" system because the term "transliterature" is not commonly understood, but correctly speaking this is a transliterature system, and definitely not Xanadu. Only transliterature systems created by Nelson have the right to call themselves Xanadu.

## Disclaimer

Xanadu is a registered trademark. This project is in no way affiliated with Ted Nelson, the Xanadu Project or any other organization with rights over the Xanadu concept. Commonplace is an independent attempt to implement the concept of a hypertext/hypermedia/transliterature system as described by Ted Nelson in his various publications.
