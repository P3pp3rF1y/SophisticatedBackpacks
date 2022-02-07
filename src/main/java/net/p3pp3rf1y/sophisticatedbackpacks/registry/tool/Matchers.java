package net.p3pp3rf1y.sophisticatedbackpacks.registry.tool;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import net.minecraft.core.Registry;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.SerializationTags;
import net.minecraft.tags.Tag;
import net.minecraft.util.GsonHelper;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.TamableAnimal;
import net.minecraft.world.entity.animal.Animal;
import net.minecraft.world.entity.animal.Bee;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.BaseRailBlock;
import net.minecraftforge.fml.ModList;
import net.minecraftforge.items.CapabilityItemHandler;
import net.minecraftforge.registries.ForgeRegistries;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedcore.util.WorldHelper;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;

public class Matchers {
	private Matchers() {}

	private static final List<ItemMatcherFactory> ITEM_MATCHER_FACTORIES = new ArrayList<>();
	private static final List<IMatcherFactory<BlockContext>> BLOCK_MATCHER_FACTORIES = new ArrayList<>();
	private static final List<IMatcherFactory<Entity>> ENTITY_MATCHER_FACTORIES = new ArrayList<>();

	static {
		addItemMatcherFactory(new ItemMatcherFactory("tag") {
			@Override
			protected Optional<Predicate<ItemStack>> getPredicateFromObject(JsonObject jsonObject) {
				String tagName = GsonHelper.getAsString(jsonObject, "tag");
				Tag<Item> tag = SerializationTags.getInstance().getOrEmpty(Registry.ITEM_REGISTRY).getTag(new ResourceLocation(tagName));
				return tag == null ? Optional.empty() : Optional.of(new ItemTagMatcher(tag));
			}
		});

		addItemMatcherFactory(new ItemMatcherFactory("emptynbt") {
			@Override
			protected Optional<Predicate<ItemStack>> getPredicateFromObject(JsonObject jsonObject) {
				ResourceLocation itemName = new ResourceLocation(GsonHelper.getAsString(jsonObject, "item"));
				if (!ForgeRegistries.ITEMS.containsKey(itemName)) {
					SophisticatedBackpacks.LOGGER.debug("{} isn't loaded in item registry, skipping ...", itemName);
				}
				Item item = ForgeRegistries.ITEMS.getValue(itemName);
				return Optional.of(st -> st.getItem() == item && (st.getTag() == null || st.getTag().isEmpty()));
			}
		});

		BLOCK_MATCHER_FACTORIES.add(new IMatcherFactory<>() {
			@Override
			public boolean appliesTo(JsonElement jsonElement) {
				return jsonElement.isJsonPrimitive();
			}

			@Override
			public Optional<Predicate<BlockContext>> getPredicate(JsonElement jsonElement) {
				String modId = jsonElement.getAsString();
				if (!ModList.get().isLoaded(modId)) {
					SophisticatedBackpacks.LOGGER.debug("{} mod isn't loaded, skipping ...", modId);
					return Optional.empty();
				}

				return Optional.of(new ModMatcher<>(modId, BlockContext::getBlock));
			}
		});
		BLOCK_MATCHER_FACTORIES.add(new TypedMatcherFactory<>("all") {
			@Override
			protected Optional<Predicate<BlockContext>> getPredicateFromObject(JsonObject jsonObject) {
				return Optional.of(block -> true);
			}
		});
		BLOCK_MATCHER_FACTORIES.add(new TypedMatcherFactory<>("rail") {
			@Override
			protected Optional<Predicate<BlockContext>> getPredicateFromObject(JsonObject jsonObject) {
				return Optional.of(blockContext -> blockContext.getBlock() instanceof BaseRailBlock);
			}
		});
		BLOCK_MATCHER_FACTORIES.add(new TypedMatcherFactory<>("item_handler") {
			@Override
			protected Optional<Predicate<BlockContext>> getPredicateFromObject(JsonObject jsonObject) {
				return Optional.of(blockContext -> WorldHelper.getBlockEntity(blockContext.getWorld(),
						blockContext.getPos()).map(te -> te.getCapability(CapabilityItemHandler.ITEM_HANDLER_CAPABILITY).isPresent()).orElse(false));
			}
		});
		ENTITY_MATCHER_FACTORIES.add(new TypedMatcherFactory<>("animal") {
			@Override
			protected Optional<Predicate<Entity>> getPredicateFromObject(JsonObject jsonObject) {
				return Optional.of(Animal.class::isInstance);
			}
		});
		ENTITY_MATCHER_FACTORIES.add(new TypedMatcherFactory<>("living") {
			@Override
			protected Optional<Predicate<Entity>> getPredicateFromObject(JsonObject jsonObject) {
				return Optional.of(LivingEntity.class::isInstance);
			}
		});
		ENTITY_MATCHER_FACTORIES.add(new TypedMatcherFactory<>("bee") {
			@Override
			protected Optional<Predicate<Entity>> getPredicateFromObject(JsonObject jsonObject) {
				return Optional.of(Bee.class::isInstance);
			}
		});
		ENTITY_MATCHER_FACTORIES.add(new TypedMatcherFactory<>("tameable") {
			@Override
			protected Optional<Predicate<Entity>> getPredicateFromObject(JsonObject jsonObject) {
				return Optional.of(TamableAnimal.class::isInstance);
			}
		});
	}

	static void addItemMatcherFactory(ItemMatcherFactory matcherFactory) {
		ITEM_MATCHER_FACTORIES.add(matcherFactory);
	}

	public static Optional<Predicate<ItemStack>> getItemMatcher(JsonElement jsonElement) {
		for (ItemMatcherFactory itemMatcherFactory : Matchers.ITEM_MATCHER_FACTORIES) {
			if (itemMatcherFactory.appliesTo(jsonElement)) {
				return itemMatcherFactory.getPredicate(jsonElement);
			}
		}
		return Optional.empty();
	}

	public static List<IMatcherFactory<BlockContext>> getBlockMatcherFactories() {
		return BLOCK_MATCHER_FACTORIES;
	}

	public static List<IMatcherFactory<Entity>> getEntityMatcherFactories() {
		return ENTITY_MATCHER_FACTORIES;
	}
}
