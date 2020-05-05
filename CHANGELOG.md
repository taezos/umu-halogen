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
