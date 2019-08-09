-record(nova_blog_author, {
                           name :: binary(),
                           about :: binary()
                          }).

-record(nova_blog_entry, {
                          title :: binary(),
                          text :: binary(),
                          author :: #nova_blog_author{},
                          date :: term()
                         }).
