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
			<h3><?php the_time(__('F jS, Y','ClearBlue2.0')) ?></h3>
		</div>
		<div class="entrybody">
			<?php the_content(__('Read the rest of this entry','ClearBlue2.0').' &raquo;'); ?>
		</div>
		
		<div class="entrymeta">
		<div class="postinfo">
			<div class="postedby"><?php printf(__('Posted by %s','ClearBlue2.0'), get_the_author()) ?></div>
			<div class="filedto"><?php printf(__('Filed in %s','ClearBlue2.0'), get_the_category_list(', ')) ?> <?php edit_post_link(__('Edit','ClearBlue2.0'), ' <span>|</span> ', ''); ?></div>
		</div>
		<?php comments_popup_link(__('No Comments','ClearBlue2.0').' &#187;', __('1 Comment','ClearBlue2.0').' &#187;', __('% Comments','ClearBlue2.0').' &#187;', 'commentslink'); ?>
		</div>
		
	</div>
	<div class="commentsblock">
		<?php comments_template(); ?>
	</div>
		<?php endwhile; ?>
		<div class="pagenavi">
			<div class="previous"><?php next_posts_link('&laquo; '.__('Previous Entries','ClearBlue2.0')) ?></div>
			<div class="next"><?php previous_posts_link(__('Next Entries','ClearBlue2.0').' &raquo;') ?></div>
		</div>
		
	<?php else : ?>

		<h2><?php _e('Not Found','ClearBlue2.0'); ?></h2>
		<div class="entrybody"><?php _e('Sorry, but you are looking for something that isn\'t here.','ClearBlue2.0'); ?></div>

	<?php endif; ?>
	
  </div>


  <?php get_sidebar(); ?>


  <?php get_footer(); ?>
</div>
</body>
</html>


