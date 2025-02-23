# minitools architecture demo

This code is a demonstration of my [proposed architecture for Halogen applications](https://dicioccio.fr/topics/purescript-halogen-architecture.html).

See it live [on my blog](https://dicioccio.fr/purescript-minitools-architecturedemo.html).

# Do not miss

- The file layout in `src`.
- State organization.
  - the use of Minitools.Seqnum in many places
  - split of in-memory State into entities, and ui items in `src/Pages/Todolist/State.purs`
  - split of `StoredState` and `State`
- Handler logic in `src/Pages/Todolist/Handlers.purs`.
  - usage of `H.modify_ (\st0 -> ...)` and utility functions to modify in-place or append-and-remove from list
  - usage `Seqnum.allocate`
  - implementation and explicit calls to `requestSave`
- The tracer implementations in `src/Main.purs`
  - for storing state
  - for reporting analytics based on application actions
  - for reporting analytics based on DOM events (Minitools.PageEvents)
- The rendering functions such as `render_notes_note state note` in `src/Pages/Todolist.purs`
  - collect rough html tree structure in the name
  - collect arguments like `state` and `note` along the tree
  - the use `reportKey` to annotate DOM events
- The definition of a widget in `src/Widgets/NoteEditor.purs`
  - the Props type, using its own "domain" of actions
  - the render function
  - where it is called in `src/Pages/Todolist.purs`

# what to expect next

Note: you can contribute individual commits to these:

- I'd like to further extract "domain" logic and some Handler-logic into separate functions (and showing how to test these pieces of code in isolation)
- add some sorting/filtering to the table

# build it

```console
spago bundle --outfile my-todo-app.js
```

## Last built with:

- spago-0.93.29
- purs-0.15.15
