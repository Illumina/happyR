#!/usr/bin/env groovy

node('uk_centos6_cluster') {

    env.PATH = "/illumina/thirdparty/R/R-3.2.3/bin:${env.PATH}"
    env._R_CHECK_FORCE_SUGGESTS_ = false

    // centos 6.5 compiled pandoc binary
    env.RSTUDIO_PANDOC = '/illumina/development/curium/bin'
    
    stage('Checkout') {
        checkout scm
    }

    stage('Build') {
        sh "R CMD build ."
    }

    stage('Test') {
        sh "R CMD check --no-examples happyR_*.tar.gz"
    }

    stage('Document') {
        // run pkgdown to generate static site and copy to webserver
        // sh "Rscript -e 'pkgdown::build_site()' && rm -rf /illumina/development/www/python/happyr/static/* || true"
        sh 'cp -r docs/* /illumina/development/www/python/happyr/static//.'
        sh 'cp -r examples /illumina/development/www/python/happyr/static/.'
        sh 'chmod a+w -Rf /illumina/development/www/python/happyr/static/ || true'
        
    }
    
    stage('Cleanup') {
        deleteDir()
    }

}