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

## How it Works

The Commonplace "docuverse" consists of files called "leaves". Each leaf is a signed, immutable file with a SHA name that can theoretically hold any content (currently limited to ASCII text only). The leaf header holds both the signature and type information for the file. In a complete system, leaves would be hosted on public servers. Because leaves are signed, immutable and content addressable, they do not need to be hosted on a particular server. A distributed hosting service such as IPFS would be adequate for hosting leaves.

Most leaves just hold content (say, the contents of a book, or the text of your homepage). Some leaves contain xanadocs, which are the hypertext documents that drive Commonplace. A xanadoc has two parts: an Edit Decision List (EDL) which defines the contents of the page, and an Overlay Decision List (ODL) which adds xanalinks. The EDL brings in snippets of content from other leaves, which are assembled by a "browser" into the contents of the page. This is similar to the way HTML pages draw in images, videos etc. held in separate files to create the complete rendered page, except that all content, even text, is held externally and can be reused in other xanadocs.

Xanalinks are another special kind of leaf. The "browser" will download all xanalinks mentioned in the ODL and apply them to the page. Xanalinks have a huge number of uses, making them difficult to explain, but the essential concept is that they mark out sections of content as having some kind of special significance. A simple example is their use for markup. A xanalink may point to a particular section of content to mark it out as needing to be rendered in italics, or to be formatted like a quotation. Xanalinks can also provide semantic information, such as marking the title of a book or the beginning and end of chapters. Most interestingly though, a xanalink can link multiple pieces of content together to show a relationship between them. For example, they may mark one piece of content as being a more up-to-date version of another one (hence creating a versioning system), or they may show that one piece of content is a translation of another. A quotation xanalink may not only mark a piece of content as needing to be formatted like a quotation, but also link back to the section of the original xanadoc that is being quoted. This allows the "browser" to provide the ability to navigate back to the original if the user desires to check the source. Imagine if all news articles contained xanalinks that allowed you to check the original sources!

## Disclaimer

Xanadu is a registered trademark. This project is in no way affiliated with Ted Nelson, the Xanadu Project or any other organization with rights over the Xanadu concept. Commonplace is an independent attempt to implement the concept of a hypertext/hypermedia/transliterature system as described by Ted Nelson in his various publications.
