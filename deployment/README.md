## Deployment

The current folder defines a nixops network that deploys a node `agora` running
the Tezos Node in a Docker container.

The current folder defines deployment of our staging infrastructure.

There are three major components at work here:

1. `configuration.nix` defines the logical machine configuration.
2. `nixops-libvirt.nix` defines how to use `nixops` to deploy this configuration
   to a local KVM VM using libvirt. See below for how to set up libvirt for this.
3. The `terraform` folder defines our cloud infrastructure.
4. The `aws.nix` file that wraps the machine config with common AWS VM options.

### Docker images

The images are configured entirely through environment variables, injected into
config files using `envsubst`. None of them have default values and all are
mandatory.

The only ports that need to be exposed are for HTTP and HTTPS on the frontend
container, as all traffic goes through `nginx`.

You can see a full deployment in `configuration.nix`.

#### Backend container

* `POSTGRES_HOST, POSTGRES_DB, POSTGRES_USER, POSTGRES_PASSWORD`: configure
  connection to the database
* `API_PORT`: Which port the backend API should bind to
* `NODE_HOST, NODE_PORT`: Hostname and port of the Tezos backend
* `DISCOURSE_API_TOKEN`: Discourse API token for posting

If your environment management is not fit for handling sensitive data, you can
bind mount an additional YAML file, and pass it to `CMD` as such:

```
docker run registry.gitlab.com/tezosagora/agora/backend \
  --volume /root/secret.yaml:/secret.yaml \
  -c /secret.yaml
```

Please note that the environment variables being empty will result in invalid
YAML and a parsing error. Set them to a bogus string or null, and the extra
config file will override the placeholder values.

#### Frontend container

* `API_HOST, API_PORT`: Hostname and port of the Agora backend
* `DNS_DOMAIN`: Domain used by Caddy to request an ACME certificate

### CI and CD

* Everything is build with `nix-build`. See file `default.nix` in the parent
  folder for details on what exactly is built in CI.
* CI uploads docker images to the
  [registry](https://gitlab.com/tezosagora/agora/container_registry) upon
  successful build.
* The CD script `scripts/deploy.sh` will update the staging machine, and restart
  the docker containers to force them to pull from the registry. Upon restarting
  the `node` container, it might take a few minutes for it to rejoin the chain
  successfully.

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

### Local development deployment with Nixops

```sh
$ cd nixops
$ nix-shell
λ nixops create -d agora-vm '<local/nixops-libvirt.nix>'
λ nixops deploy
```

The machine will be available on `http://agora`, thanks to libvirt DNS.

For further nixops usage, see `man nixops`, [The Nixops
Manual](https://nixos.org/nixops/manual/), and [The Notion
Page](https://www.notion.so/serokell/Nix-for-Dummies-64c929a69788435fa7e2c5ed65fa7604).

### Terraform

We manage all cloud infrastructure with [Terraform](https://www.terraform.io/),
an infrastructure-as-code tool.

The cluster config is in `terraform/staging.tf`.

To deploy or update the cluster:

* `terraform` is available with the necessary plugins via `shell.nix` in this folder.
* See [this document](https://docs.aws.amazon.com/cli/latest/userguide/cli-chap-configure.html) for how to set up your AWS credentials.
* Copy `.envrc.example` to `.envrc`, fill in the AWS profile name, and run `direnv allow`.

The `direnv` tool will now automatically load the `nix-shell` environment
whenever you `cd` into this folder or any subfolder, and automatically reload it
when necessary.

```sh
# You only need to run this once to create the .terraform state folder
$ terraform init
Initializing the backend...
.... more output

# Then, run this to manifest your cluster
$ terraform apply
.... bunch of output
..........
```

Note that state and locking is managed remotely via S3/DynamoDB entities that
are created with this very configuration. You might need to comment out the
first couple lines in `staging.tf` for a first deployment, and then uncomment
them and run `terraform init` to migrate state to S3. Ask ops for an AWS token
with access to the Agora Staging AWS account if you need one.
