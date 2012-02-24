
<div id="footersection">
    <div id="archive">
        <h2>Archives</h2>
        <ul class="sidebarlists">
        <?php wp_get_archives('type=monthly'); ?>
        </ul>
    </div>
    <div id="meta">
        <h2>Meta</h2>
        <ul class="sidebarlists">
        <?php wp_register(); ?>
        <li><?php wp_loginout(); ?></li>
        <li><a href="<?php bloginfo('rss2_url'); ?>" title="RSS 2.0 Feed">RSS 2.0 Feed</a></li>
        <li><a href="<?php bloginfo('comments_rss2_url'); ?>" title="RSS 2.0 Feed">RSS 2.0 Comments</a></li>
        <?php wp_meta(); ?>
        </ul>
    </div>
    <div id="links">
        <h2>Links</h2>
        <ul class="sidebarlists">
        <?php get_links('-1', '<li>', '</li>', '', 0, 'name', 0, 0, -1, 0); ?>
        </ul>
    </div>
</div>
<div id="footer"><p><?php printf(__('%s is using the <a href="http://3fact.com">ClearBlue2.0 theme</a> designed by <a href="http://3fact.com">3fact.com</a>. Powered by <a href="http://wordpress.org/">WordPress</a>','ClearBlue2.0'), get_bloginfo('name')) ?>
<!-- <?php echo get_num_queries(); ?> <?php _e('queries.','ClearBlue2.0'); ?> <?php timer_stop(1); ?> <?php _e('seconds.','ClearBlue2.0'); ?> -->
<?php wp_footer(); ?>
</p></div>


