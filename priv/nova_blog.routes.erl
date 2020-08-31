#{prefix => "",
  security => false,
  routes => [
             {"/", {nova_blog_main_controller, index}, #{}},
             {"/blog", {nova_blog_blog_controller, index}, #{}},
             {"/release/[:version]",{nova_blog_release_controller, index}, #{}},
             {"/article/[:id]",{nova_blog_article_controller,index},#{}},
             {"/docs/[:page]", {nova_blog_documentation_controller, index}, #{}},
             {"/community",{nova_blog_community_controller,index},#{}},
             {"/forum",{nova_blog_forum_controller,index},#{}},
             {404, {nova_blog_main_controller, not_found}}
            ],
  statics => [
              {"/assets/[...]", "assets"}
             ]
 }.
