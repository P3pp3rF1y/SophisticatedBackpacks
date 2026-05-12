package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import com.mojang.serialization.MapCodec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.ItemStackTemplate;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.PlacementInfo;
import net.minecraft.world.item.crafting.Recipe;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.item.crafting.SmithingRecipeInput;
import net.minecraft.world.item.crafting.display.RecipeDisplay;
import net.minecraft.world.item.crafting.display.SlotDisplay;
import net.minecraft.world.item.crafting.display.SmithingRecipeDisplay;
import net.neoforged.fml.util.thread.SidedThreadGroups;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.crafting.ICustomSmithingRecipe;
import org.jspecify.annotations.Nullable;

import java.util.List;
import java.util.Optional;

public class SmithingBackpackUpgradeRecipe implements ICustomSmithingRecipe {
	public static final MapCodec<SmithingBackpackUpgradeRecipe> MAP_CODEC = RecordCodecBuilder.mapCodec(builder -> builder.group(
			Recipe.CommonInfo.MAP_CODEC.forGetter(recipe -> recipe.commonInfo),
			Ingredient.CODEC.optionalFieldOf("template").forGetter(recipe -> recipe.template),
			Ingredient.CODEC.fieldOf("base").forGetter(recipe -> recipe.base),
			Ingredient.CODEC.optionalFieldOf("addition").forGetter(recipe -> recipe.addition),
			ItemStackTemplate.CODEC.fieldOf("result").forGetter(recipe -> recipe.resultTemplate)
	).apply(builder, SmithingBackpackUpgradeRecipe::new));
	public static final StreamCodec<RegistryFriendlyByteBuf, SmithingBackpackUpgradeRecipe> STREAM_CODEC = StreamCodec.composite(
			Recipe.CommonInfo.STREAM_CODEC,
			recipe -> recipe.commonInfo,
			Ingredient.OPTIONAL_CONTENTS_STREAM_CODEC,
			recipe -> recipe.template,
			Ingredient.CONTENTS_STREAM_CODEC,
			recipe -> recipe.base,
			Ingredient.OPTIONAL_CONTENTS_STREAM_CODEC,
			recipe -> recipe.addition,
			ItemStackTemplate.STREAM_CODEC,
			recipe -> recipe.resultTemplate,
			SmithingBackpackUpgradeRecipe::new
	);
	public static final RecipeSerializer<SmithingBackpackUpgradeRecipe> SERIALIZER = new RecipeSerializer<>(MAP_CODEC, STREAM_CODEC);

	private final Recipe.CommonInfo commonInfo;
	final Optional<Ingredient> template;
	final Ingredient base;
	final Optional<Ingredient> addition;
	final ItemStackTemplate resultTemplate;
	@Nullable
	private PlacementInfo placementInfo;

	public SmithingBackpackUpgradeRecipe(Optional<Ingredient> template, Ingredient base, Optional<Ingredient> addition, ItemStack result) {
		this(new Recipe.CommonInfo(true), template, base, addition, ItemStackTemplate.fromNonEmptyStack(result));
	}

	public SmithingBackpackUpgradeRecipe(Recipe.CommonInfo commonInfo, Optional<Ingredient> template, Ingredient base, Optional<Ingredient> addition, ItemStackTemplate resultTemplate) {
		this.commonInfo = commonInfo;
		this.template = template;
		this.base = base;
		this.addition = addition;
		this.resultTemplate = resultTemplate;
	}

	@Override
	public boolean isSpecial() {
		return true;
	}

	@Override
	public ItemStack assemble(SmithingRecipeInput inv) {
		ItemStack upgradedBackpack = resultTemplate.create();
		if (Thread.currentThread().getThreadGroup() == SidedThreadGroups.SERVER) {
			getBackpack(inv).map(ItemStack::getComponentsPatch).ifPresent(upgradedBackpack::applyComponents);
			IBackpackWrapper wrapper = BackpackWrapper.fromStack(upgradedBackpack);
			BackpackItem backpackItem = ((BackpackItem) upgradedBackpack.getItem());
			wrapper.setSlotNumbers(backpackItem.getNumberOfSlots(), backpackItem.getNumberOfUpgradeSlots());
		}
		return upgradedBackpack;
	}

	private Optional<ItemStack> getBackpack(SmithingRecipeInput inv) {
		ItemStack slotStack = inv.getItem(1);
		if (slotStack.getItem() instanceof BackpackItem) {
			return Optional.of(slotStack);
		}
		return Optional.empty();
	}

	@Override
	public RecipeSerializer<SmithingBackpackUpgradeRecipe> getSerializer() {
		return ModItems.SMITHING_BACKPACK_UPGRADE_RECIPE_SERIALIZER.get();
	}

	public Optional<Ingredient> templateIngredient() {
		return template;
	}

	public Ingredient baseIngredient() {
		return base;
	}

	public Optional<Ingredient> additionIngredient() {
		return addition;
	}

	public Ingredient getTemplateIngredient() {
		return template.orElse(Ingredient.of(Items.AIR));
	}

	public Ingredient getBaseIngredient() {
		return base;
	}

	public Ingredient getAdditionIngredient() {
		return addition.orElse(Ingredient.of(Items.AIR));
	}

	public PlacementInfo placementInfo() {
		if (placementInfo == null) {
			placementInfo = PlacementInfo.createFromOptionals(List.of(template, Optional.of(base), addition));
		}

		return placementInfo;
	}

	public List<RecipeDisplay> display() {
		return List.of(new SmithingRecipeDisplay(
				Ingredient.optionalIngredientToDisplay(template),
				base.display(),
				Ingredient.optionalIngredientToDisplay(addition),
				new SlotDisplay.ItemStackSlotDisplay(resultTemplate),
				new SlotDisplay.ItemSlotDisplay(Items.SMITHING_TABLE)
		));
	}

	@Override
	public ItemStack result() {
		return resultTemplate.create();
	}

	@Override
	public boolean showNotification() {
		return commonInfo.showNotification();
	}

	@Override
	public String group() {
		return "";
	}

}
