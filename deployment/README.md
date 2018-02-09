# Deployment automation for æternity nodes

## Ansible Deploy

This implementation supports OpenStack dynamic inventory by using [Ansible 2.4 OpenStack inventory plugin](https://docs.ansible.com/ansible/devel/plugins/inventory/openstack.html).
To install it's dependencies with `pip`:
```bash
pip install -r ansible/pip-requirements.txt
```

You should make sure [OpenStack credentials are set](https://docs.openstack.org/python-openstackclient/latest/configuration/index.html#environment-variables)
either by environment variables or clouds.yml file.

```bash
source ~/my/secrets/openstack.rc
ansible-inventory -i inventory/openstack.yml --list
```

Make sure your OpenStack compute instances has correct "groups" metadata set.
For example to add an instance to both groups "epoch" and "integration" use:
```bash
nova meta my_instance_name set groups="epoch,integration"
```

Deploy targets (hosts) can be filtered by using [Ansible host patterns](http://docs.ansible.com/ansible/latest/intro_patterns.html).
The example below limits the playbook run to hosts in **both** epoch and integration groups.
```bash
ansible-playbook -i inventory/openstack.yml --limit="epoch:&integration"
```

Deploy playbook supports local or remote build packages.
```bash
ansible-playbook --extra-vars "local_package=/path/to/epoch-0.2.0.tar.gz" deploy.yml
```

Full example deploying remote package from aeternity/epoch GitHub releases
to all OpenStack instances tagged (meta) with groups "epoch" **and** "integration".
Make sure you have SSH access to "epoch" remote user.

```bash
PACKAGE="https://github.com/aeternity/epoch/releases/download/v0.2.0-good-peers/ubuntu-epoch-0.2.0.tar.gz"
cd ansible && ansible-playbook -i inventory/openstack.yml --limit="epoch:&integration" \
   --extra-vars "package=$PACKAGE" deploy.yml
```

### Secrets

Secrets are managed with [Ansible Vault](docs.ansible.com/ansible/2.4/vault.html).
There is a tiny bridge vault file `vault-env` that bridges the `ANSIBLE_VAULT_PASSWORD` environment variable as Ansible vault password.

```
export ANSIBLE_VAULT_PASSWORD="top secret"
ansible-playbook deploy.yml
```
