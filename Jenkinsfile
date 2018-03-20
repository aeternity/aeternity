// This build file for Jenkins is meant to create a new test network from the
// latest master on a daily basis.

pipeline {
  agent any

  triggers {
    cron('@daily')
  }

  environment {
    IMAGE_NAME = 'aetrnty/epoch'
    COMPOSE_FILE = 'docker-testnet.yml'
  }

  stages {
    stage('Docker build') {
      steps { 
        script {
          docker.build("${IMAGE_NAME}:${env.BUILD_ID}").tag('latest')
        }
      }
    }

    stage('(Re-)Create network') {
      steps {
        sh "docker-compose -f ${COMPOSE_FILE} down"
        sh "docker-compose -f ${COMPOSE_FILE} up --no-build -d"
      }
    }
  }
}
