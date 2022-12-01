assert ! ( new File( basedir, "build.log" ).text
        .contains( 'Property ${artifact-version}: Set of valid available versions is [1.0, 1.0.1,' ) )
