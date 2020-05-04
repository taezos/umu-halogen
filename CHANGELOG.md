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
