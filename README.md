# Bvldr

## Architecture
Bvldr consists out of several components:

### Listener
Listen for the following events on github:

- PushEvent
- PullRequestCreatedEvent

When such a event occurs,  the listener forwards the following messages to the core:

- AddCommitRef

### Core
The Core system routes the requests to the correct build node.  It supports the following messages.

#### Node management
- AddBuildNode
- RemoveBuildNode
- Pong

#### Repository management
- AddCustomer
- RemoveCustomer
- AddCommitRef

### Build node
The build node is the component that executes a command on a node.  It supports the following messages:

- Ping
- RunCommand
