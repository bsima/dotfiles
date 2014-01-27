PERL5LIB=${PERL5LIB}:${HOME}/bio/bioperl-1.2.3
PERL5LIB=${PERL5LIB}:${HOME}/bio/ensembl/modules
PERL5LIB=${PERL5LIB}:${HOME}/bio/ensembl-compara/modules
PERL5LIB=${PERL5LIB}:${HOME}/bio/ensembl-variation/modules
PERL5LIB=${PERL5LIB}:${HOME}/bio/ensembl-functgenomics/modules
export PERL5LIB

export EDITOR='vim'

# ## Amazon AMI Tools ##
#
# Not sure if this is the best place for it but whatevs.
export JAVA_HOME="$(/usr/libexec/java_home)"
export EC2_PRIVATE_KEY="$(/bin/ls "$HOME"/.ec2/pk-*.pem | /usr/bin/head -1)"
export EC2_CERT="$(/bin/ls "$HOME"/.ec2/cert-*.pem | /usr/bin/head -1)"
export EC2_AMITOOL_HOME="/usr/local/Cellar/ec2-ami-tools/1.4.0.9/libexec"
