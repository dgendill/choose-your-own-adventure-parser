# choose-your-own-adventure-parser

Command line tool and codebase for parsing choose your own adventures files. [See here](https://github.com/dgendill/choose-your-own-adventure-maker) for specification and UI for making the files.

### Get Started

Checkout the repo and run `npm install`. To compile a choose-your-own-adventure-file run

```
node story-compiler.js adventure --file path/to/file.str --to dist
```

For example...

```
node story-compiler.js adventure --file examples/ShortStory.str --to dist
```

Then you can view the example story in the browser by running `npm run server -- dist`.

### Development

To compile the command line tool run...

```
npm run compile-command-line
```

This will create `story-compiler.js` in the root folder.

---

To compile the front end driver, run `npm run compile`.  This will create the `main.js` in the dev folder.
