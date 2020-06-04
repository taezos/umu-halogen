# Revision history for umu-halogen

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.

## v0.2.0.0 -- 2020-05-04

### Feautres.
* Generate a Component.
- generate a component with command `umu-halogen <path> <component_name>`.

### Refactors
- renamed `UmuHaogen.Capability.ManageCommand`  to `UmuHalogen.Capability.Command`
- moved `UmuHalogen.Command` to `UmuHalogen.Parser.Command`
- removed `microlens-th`
- derived lenses manually.

## v0.2.0.1 -- 2020-05-05

### Refactors
* directory generation response handler refactored into one function and
placed in UmuHalogen.Util, it was repeated with every directory
generation function.

* directory generation function had dirName as hardcoded texts scattered
in the function. Refactored as `dirName` in where clause.

* directory existence check is scarred on every directory generation function.
It has been refactored to `isDirGenerated` function and placed in UmuHalogen.Util

* file generations functions had repeated if..then..else lines. Refactored
to `generateWhenFilenotExists` function. This function is stored in UmuHalogen.Util

* file generation line was repeated in every file generation function,
refactored to `generateFile`, including `logInfo ( "Generated " <> filePath )`

* "Generating" changed to "Generated", and ellipsis were removed

* every function in file generation had the file path string scattered
  in the function, it is now contained in the where clause

* ever file generation have a file checking line where it checks for the
  existence of the file. That line is now contained in a function and
  stored in UmuHalogen.Util.

* every function in file generation had the file path string scattered
  in the function, it is now contained in the where clause

* ever file generation have a file checking line where it checks for the
  existence of the file. That line is now contained in a function and
  stored in UmuHalogen.Util.

* directory generation function had dirName as hardcoded texts scattered
in the function. Refactored as `dirName` in where clause.

## v0.2.0.2 -- 2020-05-15

* The spago file  project name field was previously hardcoded. Refactored it to take use
the parent directory name as project name

* renamed UmuHalogen.Capability.LogMessage to UmuHalogen.Capability.Log 

* Generated component now has `Slot` and `SProxy`. 

* `toPascalCase` is applied on the input for component name.

* Template files added to cabal file so the project can be install with `cabal
  new-install`. Thanks to @chiroptical.

* Fixed generated component typo. Thanks to @chiroptical.

* Refactored directory generation as `dirResHandler`. This logic was previously
  littered in every directory generation function.

## v0.2.0.3 -- 2020-05-18

### Update
* routing is now part of initial generation.
* Home and About page are generated on initial project generation. These 2 pages
  are imported in the router component.
* It will generate `AppM.purs`. It will contain the bare representation of the application
* Updated `Main.purs`. It initializes the router.

## Refactor

* Refactored file and directory generation to only contain the information about the file to
  be generated. The generation function is now separated.

## v0.2.0.4 -- 2020-06-04

### Features

* `umu-halogen route [RELATIVE_PATH_TO_PROJECT] [ROUTE_NAME]` will generate a
  new route. It will add the `[ROUTE_NAME]` as a data constructor to `data
  Route`, and also update the `routeCoded` with the new route.

### Refactor

* umu-halogen is now in it's own directory, on the same level as
  `purescript-ast` and `purescript-cst`. Then a `cabal.project` coordinates all
  of these projects.

* Extracted `purescript-ast` and `purescript-cst`, and deleted `purescript`
  private dependencies. This was done so it was easier to build with nix.

* `mkModuleName` fixed. `Module [ ProperName Namespace ]` is changed to `Module Text`.

* created newtypes like `LinesState` instead of relying on booleans whether code 
  needs to go to a new line. `SrcLineHead` to be more descriptive instead of
  just using `[ Int ]`.

* switched from `classy-prelude` to `relude`.

* Updated app representation to include `ReaderT`.

* Created a sum type that represents all successful operations.

* Created a sum type that represents all fail operations.

* Errors are propagated all the way to the surface of the app, and all error
  handling is done there. 

* path parsing is updated to use parsec.
