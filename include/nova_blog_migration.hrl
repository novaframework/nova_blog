-define(NOVA_BLOG_TABLES, [<<"CREATE TABLE IF NOT EXISTS \"nova_blog_author\"">>,
                           <<"(">>,
                           <<"  \"id\" smallserial NOT NULL,">>,
                           <<"  \"name\" varchar(50),">>,
                           <<"  \"email\" varchar(50),">>,
                           <<"  \"description\" text">>,
                           <<")">>,
                           <<";">>,
                           <<"">>,
                           <<"ALTER TABLE \"nova_blog_author\" ADD CONSTRAINT \"id\"">>,
                           <<"  PRIMARY KEY (\"id\")">>,
                           <<";">>,
                           <<"">>,
                           <<"CREATE TABLE IF NOT EXISTS \"nova_blog_entry\"">>,
                           <<"(">>,
                           <<"  \"id\" smallserial NOT NULL,">>,
                           <<"  \"title\" varchar(100),">>,
                           <<"  \"content\" text,">>,
                           <<"  \"author_id\" smallserial NOT NULL,">>,
                           <<"  \"added\" timestamp DEFAULT CURRENT_TIMESTAMP">>,
                           <<")">>,
                           <<";">>,
                           <<"">>,
                           <<"ALTER TABLE \"nova_blog_entry\" ADD CONSTRAINT \"pk_nova_blog_entry\"">>,
                           <<"  PRIMARY KEY (\"id\")">>,
                           <<";">>,
                           <<"">>,
                           <<"CREATE TABLE IF NOT EXISTS \"nova_blog_release\"">>,
                           <<"(">>,
                           <<"  \"id\" smallserial NOT NULL,">>,
                           <<"  \"version\" varchar(10),">>,
                           <<"  \"description\" text,">>,
                           <<"  \"added\" timestamp DEFAULT CURRENT_TIMESTAMP,">>,
                           <<"  \"author_id\" smallserial NOT NULL">>,
                           <<")">>,
                           <<";">>,
                           <<"">>,
                           <<"ALTER TABLE \"nova_blog_release\" ADD CONSTRAINT \"pk_nova_blog_release\"">>,
                           <<"  PRIMARY KEY (\"id\")">>,
                           <<";">>,
                           <<"">>,
                           <<"ALTER TABLE \"nova_blog_entry\" ADD CONSTRAINT \"fk_nova_blog_entry_\"">>,
                           <<"  FOREIGN KEY (\"author_id\") REFERENCES \"nova_blog_author\" (\"id\") ON DELETE NO ACTION ON UPDATE NO ACTION">>,
                           <<";">>,
                           <<"">>,
                           <<"ALTER TABLE \"nova_blog_release\" ADD CONSTRAINT \"fk_nova_blog_release_\"">>,
                           <<"  FOREIGN KEY (\"author_id\") REFERENCES \"nova_blog_author\" (\"id\") ON DELETE NO ACTION ON UPDATE NO ACTION">>,
                           <<";">>]).
