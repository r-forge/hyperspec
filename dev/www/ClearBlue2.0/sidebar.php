<div id="sidebar">
<h2><?php _e('Categories','ClearBlue2.0'); ?></h2>
<ul class="sidebarlists">
<?php wp_list_cats('sort_column=name&optioncount=0&hierarchical=1'); ?>
</ul>
<h2><?php _e('Archives','ClearBlue2.0'); ?></h2>
<ul class="sidebarlists">
<?php wp_get_archives('type=monthly'); ?>
</ul>
<?php /* If this is the frontpage */ if ( is_home() || is_page() ) { ?>

<h2>Links</h2>
<ul class="sidebarlists">
<?php get_links('-1', '<li>', '</li>', '', 0, 'name', 0, 0, -1, 0); ?>
</ul>
<h2>Meta</h2>
<ul class="sidebarlists">
<?php wp_register(); ?>
<li><?php wp_loginout(); ?></li>
<li><a href="<?php bloginfo('rss2_url'); ?>" title="RSS 2.0 Feed">RSS 2.0 Feed</a></li>
<li><a href="<?php bloginfo('comments_rss2_url'); ?>" title="RSS 2.0 Feed">RSS 2.0 Comments</a></li>
<?php wp_meta(); ?>
</ul>
<?php } ?>
<h2><?php _e('Search','ClearBlue2.0'); ?></h2>
<ul class="sidebarlists">
<li>
<?php include (TEMPLATEPATH . '/searchform.php'); ?>
</li>
</ul>
</div>


