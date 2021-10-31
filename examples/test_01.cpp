// [!>2 Lorem ipsum dolor sit amet, consectetur adipiscing elit.]

// [!.. Sed vel condimentum elit, in pulvinar lectus. Mauris dictum felis sodales est venenatis.]
// [!1F Suspendisse eu risus pretium, porta nisi vel, feugiat nisl. Nam sodales tincidunt turpis.]
// Donec neque risus, dapibus eleifend volutpat [!..]
void
 doSomething( int fd, MetaData meta )
{
    ...
}

/*
  Suspendisse eu risus pretium, porta nisi vel, feugiat nisl.
 */

// [!1F In ante sapien, sodales a semper eget, pharetra eget leo. In aliquam gravida nibh.]
// Cras eu ipsum commodo, vestibulum erat sed, accumsan diam. [!..]
void
 doSomethingFromCache( char const * binPath )
{
    // [!2F n eu luctus nisl. Mauris mollis justo vitae libero iaculis malesuada.]
    // [!.. Nunc augue ex, ultricies ac lorem a, placerat bibendum diam.]
    int fd = open( binPath, O_RDONLY );

    // [!3? Duis ullamcorper quam semper nisi dictum tincidunt.]
    if ( fd == -1 )
    {
        exit( EXIT_FAILURE );
    }

    // [!+F Mauris mauris magna, molestie sit amet quam et, ornare lacinia enim.]
    unsigned char * header[ HEADER_SIZE ];

    // Below just adds a link, like a "see also".
    // [!.C Pellentesque luctus magna sem, id commodo libero auctor vel.]

    // [!7? Morbi tempor turpis gravida, dignissim sem sit amet, feugiat justo. ]
    // Ensure cache is valid.
    if ( meta.vPerThing )
    {
        // [!.. Sed nisl arcu, interdum a pellentesque sagittis, finibus ac tortor.]
        switch ( meta.unitSize )
        {
            case 8:
                // Quisque leo enim, porta vitae elit at, euismod placerat ipsum. [!2F]
                // [!.. Duis aliquet arcu ut odio iaculis, sagittis venenatis sapien lacinia.]
                // [!.. Ut porttitor tellus vel sagittis blandit.]
                // Below turns jump into the "inner frame".
                // [!>. Jump ]

                // Optionally, it can contain a significance/complexity hint about jump target.
                // [!4C Begin ]
                // [!>2 Middle ]
                // [!<F End ]
                doSomething< double >( fd, meta );
                break;
        }
    }

    // [!3. Vivamus libero massa, malesuada ac nisi et, placerat blandit leo.]
    // [!.. Curabitur eget consequat lacus.]
    // [!+F Pellentesque nec ipsum vitae nibh congue aliquet at sit amet dolor.]
    close( fd );
}
// Maecenas odio eros, pulvinar quis gravida quis, egestas tincidunt nibh. [!0.]

// [!0F Class aptent taciti sociosqu ]



// [!>2 Nunc eros justo, semper vel mattis non, semper in turpis.]

// [!.. Morbi tempor turpis gravida, dignissim sem ]
// [!1F Lorem ipsum. ]
// Vivamus nec molestie purus. [!..]
void
 doSomething( int fd, MetaData meta )
{
    ...
}

/*
  Suspendisse eu risus pretium, porta nisi vel, feugiat nisl.
 */

// [!1F Lorem ipsum dolor sit amet.]
// Donec mollis aliquam sem eget finibus. [!..]
void
 doSomethingFromCache( char const * binPath )
{
    // [!2F Vivamus porttitor sollicitudin dui, ac aliquam mi commodo vel.]
    // [!.. Pellentesque nec ipsum vitae nibh congue aliquet at sit amet dolor.]
    int fd = open( binPath, O_RDONLY );

    // [!3? Duis a metus id erat molestie bibendum eu a mauris.]
    if ( fd == -1 )
    {
        exit( EXIT_FAILURE );
    }
