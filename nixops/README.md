## Deployment with Nix

The current folder defines a nixops network that deploys a node `agora` running
the Tezos Node in a Docker container.

`staging.nix` defines the logical network, that is, service and server
configuration, while `libvirt.nix` defines the physical network, that is, how to
actually put the logical network onto *something*. In this case, it defines how
to deploy the configuration to a KVM VM via libvirt (both of which need to be
installed and configured to allow your user RW access).

### Configuring libvirt

####
I'm told there is libvirt support for MacOS, but I don't have access to a Mac
for testing. Please ask Google.

#### NixOS
Please see [The Nixops
Manual](https://nixos.org/nixops/manual/#idm140737322394336).

#### Arch Linux

First, install `libvirt qemu ebtables dnsmasq`. Then add yourself to the `kvm`
group.

Add the following to `/etc/polkit-1/rules.d/50-libvirt.rules`:
```
/* Allow users in kvm group to manage the libvirt
daemon without authentication */
polkit.addRule(function(action, subject) {
    if (action.id == "org.libvirt.unix.manage" &&
        subject.isInGroup("kvm")) {
            return polkit.Result.YES;
    }
});
```

Enable the necessary services:
```
$ sudo systemctl enable libvirtd ebtables
```

Make sure your user can write to the system image pool:
```sh
$ sudo chgrp -R kvm /var/lib/libvirt
$ sudo chmod g+w -R /var/lib/libvirt
```

Then reboot your system.

### Usage

```sh
$ cd nixops
$ nix-shell
λ nixops create -d agora-vm '<local/staging.nix>' '<local/libvirt.nix>'
λ nixops deploy
```

For further nixops usage, see `man nixops`, [The Nixops
Manual](https://nixos.org/nixops/manual/), and [The Notion Page](https://www.notion.so/serokell/Nix-for-Dummies-64c929a69788435fa7e2c5ed65fa7604).
