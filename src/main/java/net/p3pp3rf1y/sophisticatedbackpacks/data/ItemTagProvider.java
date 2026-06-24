package net.p3pp3rf1y.sophisticatedbackpacks.data;

import net.minecraft.core.HolderLookup;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.data.PackOutput;
import net.minecraft.data.tags.TagAppender;
import net.minecraft.resources.Identifier;
import net.minecraft.world.item.Item;
import net.neoforged.neoforge.common.data.ItemTagsProvider;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

public class ItemTagProvider extends ItemTagsProvider {
	public ItemTagProvider(PackOutput packOutput, CompletableFuture<HolderLookup.Provider> lookupProvider) {
		super(packOutput, lookupProvider, SophisticatedBackpacks.MOD_ID);
	}

	@Override
	protected void addTags(HolderLookup.Provider pProvider) {
		TagAppender<Item, Item> upgradeTag = tag(ModItems.BACKPACK_UPGRADE_TAG);
		BuiltInRegistries.ITEM.entrySet().stream().filter(
				entry -> entry.getKey().identifier().getNamespace().equals(SophisticatedBackpacks.MOD_ID) && entry.getValue() instanceof UpgradeItemBase)
				.map(Map.Entry::getValue).forEach(item -> {
					Identifier location = BuiltInRegistries.ITEM.getKey(item);
					if (location.getPath().contains("/")) {
						upgradeTag.addOptional(item);
					} else {
						upgradeTag.add(item);
					}
				});
	}
}
