## Deployment

The current folder defines a nixops network that deploys a node `agora` running
the Tezos Node in a Docker container.

The current folder defines deployment of our staging infrastructure.

There are three major components at work here:

1. `configuration.nix` defines the logical machine configuration.
2. `nixops-libvirt.nix` defines how to use `nixops` to deploy this configuration
   to a local KVM VM using libvirt.
3. The `terraform` folder defines our cloud infrastructure.

### Configuring libvirt

#### MacOS
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
λ nixops create -d agora-vm '<local/nixops-libvirt.nix>'
λ nixops deploy
```

For further nixops usage, see `man nixops`, [The Nixops
Manual](https://nixos.org/nixops/manual/), and [The Notion Page](https://www.notion.so/serokell/Nix-for-Dummies-64c929a69788435fa7e2c5ed65fa7604).
