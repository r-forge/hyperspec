<?php get_header(); ?>
<div id="wrap">
  <div id="content">
	<?php if (have_posts()) :?>
		<?php $postCount=0; ?>
		<?php while (have_posts()) : the_post();?>
			<?php $postCount++;?>
	<div class="entry entry-<?php echo $postCount ;?>">
		<div class="entrytitle">
			<h2><a href="<?php the_permalink() ?>" rel="bookmark" title="<?php printf(__('Permanent Link to %s','ClearBlue2.0'), get_the_title()) ?>"><?php the_title(); ?></a></h2>
		</div>
		<div class="entrybody">
			<?php the_content(__('Read the rest of this entry','ClearBlue2.0').' &raquo;'); ?>
		</div>
		
		<div class="entrymeta"><?php printf(__('Filed in %s','ClearBlue2.0'), get_the_category_list(', ')) ?><span>|</span><?php edit_post_link(__('Edit','ClearBlue2.0'), '', '<span>|</span> '); ?>  <?php comments_popup_link(__('No Comments','ClearBlue2.0').' &#187;', __('1 Comment','ClearBlue2.0').' &#187;', __('% Comments','ClearBlue2.0').' &#187;'); ?>
		</div>
		
	</div>
	<div class="commentsblock">
		<?php comments_template(); ?>
	</div>
		<?php endwhile; ?>
		<div class="navigation">
			<div class="alignleft"><?php next_posts_link('&laquo; '.__('Previous Entries','ClearBlue2.0')) ?></div>
			<div class="alignright"><?php previous_posts_link(__('Next Entries','ClearBlue2.0').' &raquo;') ?></div>
		</div>
		
	<?php else : ?>

		<h2>Not Found</h2>
		<div class="entrybody"><?php _e('Sorry, but you are looking for something that isn\'t here.','ClearBlue2.0'); ?></div>

	<?php endif; ?>
	
  </div>


  <?php get_sidebar(); ?>


  <?php get_footer(); ?>
</div>
</body>
</html>


