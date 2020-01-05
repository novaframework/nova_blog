-record(nova_blog_author, {
                           name :: binary(),
                           about :: binary()
                          }).

-record(nova_blog_entry, {
                          id :: integer(),
                          title :: binary(),
                          text :: binary(),
                          author :: #nova_blog_author{},
                          date :: term()
                         }).

-record(nova_blog_release, {
                            id :: integer(),
                            version :: binary(),
                            changes :: binary(),
                            date :: term()
                           }).
