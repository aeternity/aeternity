# Deployment automation for æternity nodes

Nodes are deployed using Ansible playbooks with dynamic inventories and host limited by environment and role groups.


## Cloud Providers

### OpenStack
OpenStack dynamic inventory using [Ansible 2.4 OpenStack inventory plugin](https://docs.ansible.com/ansible/devel/plugins/inventory/openstack.html).

### Amazon Web Services
AWS (ec2) dynamic inventory using [AWS EC2 script](https://raw.githubusercontent.com/ansible/ansible/v2.5.0b1/contrib/inventory/ec2.py) and [it's documentation](http://docs.ansible.com/ansible/latest/intro_dynamic_inventory.html#example-aws-ec2-external-inventory-script)

### Dependencies
To install dynamic inventories dependencies with `pip` run:
```bash
pip install -r ansible/pip-requirements.txt
```


## Credentials

### Openstack
You should make sure [OpenStack credentials are set](https://docs.openstack.org/python-openstackclient/latest/configuration/index.html#environment-variables)
either by environment variables or clouds.yml file.

```bash
source ~/my/secrets/openstack.rc
ansible-inventory -i inventory/openstack.yml --list
```

### Amazon Web Services
Yous should make sure [AWS CommandLine interface credentials are set](http://docs.ansible.com/ansible/latest/intro_dynamic_inventory.html#example-aws-ec2-external-inventory-script) either by environment variables or ~/.aws/credentials file

```bash
ansible-inventory -i inventory/ec2.py --list
```

If you have configured multiple AWS credentials you can pass AWS_PROFILE variable before command
```bash
AWS_PROFILE=aeternity ansible-inventory -i inventory/ec2.py --list
```


## Inventory Groups

Make sure your OpenStack compute instances has correct "groups" metadata set.
For example to add an instance to both groups "tag_role_epoch" and "tag_env_integration" use:
```bash
nova meta my_instance_name set groups="tag_role_epoch,tag_env_integration"
```

Deploy targets (hosts) can be filtered by using [Ansible host patterns](http://docs.ansible.com/ansible/latest/intro_patterns.html).
The example below limits the playbook run to hosts in **both** tag_role_epoch and tag_env_integration groups.
```bash
ansible-playbook --limit="tag_role_epoch:&tag_env_integration"
```

Deploy playbook supports local or remote build packages.
```bash
ansible-playbook --extra-vars "package=/path/to/epoch-0.14.0.tar.gz" deploy.yml
```

Full example deploying remote package from [aeternity/epoch GitHub releases](https://github.com/aeternity/epoch/releases)
to all instances in groups "tag_role_epoch" **and** "tag_env_integration".
Make sure you have SSH access to "epoch" remote user.

```bash
PACKAGE="https://github.com/aeternity/epoch/releases/download/v0.14.0/epoch-0.14.0-Linux-x86_64.tar.gz"
DEPLOY_ENV=integration
cd ansible && ansible-playbook --limit="tag_role_epoch:&tag_env_${DEPLOY_ENV:?}" \
   --extra-vars "package=${PACKAGE:?} hosts_group=tag_env_${DEPLOY_ENV:?} env=${DEPLOY_ENV:?}" deploy.yml
```


## Secrets

Secrets are managed with [Ansible Vault](docs.ansible.com/ansible/2.4/vault.html).
There is a tiny bridge vault file `vault-env` that bridges the `EPOCH_ANSIBLE_VAULT_PASSWORD` environment variable as Ansible vault password.

```
export EPOCH_ANSIBLE_VAULT_PASSWORD="top secret"
ansible-playbook deploy.yml
```
