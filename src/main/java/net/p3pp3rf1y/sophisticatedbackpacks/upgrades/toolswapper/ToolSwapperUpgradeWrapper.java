package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper;

import com.google.common.collect.Multimap;
import com.google.common.util.concurrent.AtomicDouble;
import net.minecraft.block.BeehiveBlock;
import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.block.LeavesBlock;
import net.minecraft.entity.Entity;
import net.minecraft.entity.ai.attributes.Attribute;
import net.minecraft.entity.ai.attributes.AttributeModifier;
import net.minecraft.entity.ai.attributes.Attributes;
import net.minecraft.entity.ai.attributes.ModifiableAttributeInstance;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.inventory.EquipmentSlotType;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.item.ShearsItem;
import net.minecraft.util.Hand;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.common.IForgeShearable;
import net.minecraftforge.common.Tags;
import net.minecraftforge.common.ToolType;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IAttackEntityResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBlockClickResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBlockToolSwapUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IEntityToolSwapUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.registry.tool.SwordRegistry;
import net.p3pp3rf1y.sophisticatedbackpacks.registry.tool.ToolRegistry;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import javax.annotation.Nullable;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Optional;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.Predicate;

public class ToolSwapperUpgradeWrapper extends UpgradeWrapperBase<ToolSwapperUpgradeWrapper, ToolSwapperUpgradeItem>
		implements IBlockClickResponseUpgrade, IAttackEntityResponseUpgrade, IBlockToolSwapUpgrade, IEntityToolSwapUpgrade {
	private static final ToolType SWORD_TOOL_TYPE = ToolType.get("sword");

	private final ToolSwapperFilterLogic filterLogic;
	@Nullable
	private ResourceLocation toolCacheFor = null;
	private final Queue<ItemStack> toolCache = new LinkedList<>();

	protected ToolSwapperUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
		filterLogic = new ToolSwapperFilterLogic(upgrade, upgradeSaveHandler);
	}

	@Override
	public boolean onBlockClick(PlayerEntity player, BlockPos pos) {
		ToolSwapMode toolSwapMode = getToolSwapMode();
		if (player.isCreative() || player.isSpectator() || toolSwapMode == ToolSwapMode.NO_SWAP) {
			return false;
		}

		ItemStack mainHandItem = player.getMainHandItem();
		if (mainHandItem.getItem() instanceof BackpackItem || (toolSwapMode == ToolSwapMode.ONLY_TOOLS && isSword(mainHandItem, player)) || (!isSword(mainHandItem, player) && !isTool(mainHandItem))) {
			return false;
		}

		BlockState state = player.level.getBlockState(pos);
		Block block = state.getBlock();

		if (getToolTypeEffectiveOnBlock(state, block, mainHandItem).isPresent() || goodAtBreakingBlock(state, mainHandItem)) {
			return true;
		}

		return tryToSwapTool(player, state, block, mainHandItem);
	}

	private boolean tryToSwapTool(PlayerEntity player, BlockState state, Block block, ItemStack mainHandItem) {
		AtomicReference<ItemStack> selectedTool = new AtomicReference<>(ItemStack.EMPTY);
		AtomicInteger selectedSlot = new AtomicInteger(-1);
		AtomicBoolean finished = new AtomicBoolean(false);
		IItemHandlerModifiable backpackInventory = backpackWrapper.getInventoryHandler();
		InventoryHelper.iterate(backpackInventory, (slot, stack) -> {
			if (stack.isEmpty()) {
				return;
			}
			if (isAllowedAndGoodAtBreakingBlock(state, block, stack)) {
				selectedSlot.set(slot);
				selectedTool.set(stack);
				if (!(block instanceof LeavesBlock) || !getToolTypes(stack).contains(ToolType.HOE)) {
					finished.set(true);
				}
			}
		}, finished::get);
		ItemStack tool = selectedTool.get();
		if (!tool.isEmpty() && (tool.getCount() == 1 || InventoryHelper.insertIntoInventory(mainHandItem, backpackInventory, true).isEmpty())) {
			player.setItemInHand(Hand.MAIN_HAND, backpackInventory.extractItem(selectedSlot.get(), 1, false));
			InventoryHelper.insertIntoInventory(mainHandItem, backpackInventory, false);
			return true;
		}

		return false;
	}

	private boolean isAllowedAndGoodAtBreakingBlock(BlockState state, Block block, ItemStack stack) {
		return getToolTypeEffectiveOnBlock(state, block, stack).map(toolType -> filterLogic.matchesToolFilter(stack, toolType)).orElse(false) || goodAtBreakingBlock(state, stack);
	}

	private boolean goodAtBreakingBlock(BlockState state, ItemStack stack) {
		return stack.isCorrectToolForDrops(state) && stack.getDestroySpeed(state) > 1.5;
	}

	private Optional<ToolType> getToolTypeEffectiveOnBlock(BlockState state, Block block, ItemStack stack) {
		for (ToolType type : getToolTypes(stack)) {
			if (block.isToolEffective(state, type)) {
				return Optional.of(type);
			}
		}
		return Optional.empty();
	}

	private Set<ToolType> getToolTypes(ItemStack stack) {
		Set<ToolType> toolTypes = stack.getToolTypes();

		if (toolTypes.isEmpty()) {
			return ToolRegistry.getItemToolTypes(stack);
		}

		return toolTypes;
	}

	@Override
	public boolean onAttackEntity(PlayerEntity player) {
		if (!shouldSwapWeapon()) {
			return false;
		}

		ItemStack mainHandItem = player.getMainHandItem();

		if (isSword(mainHandItem, player)) {
			return true;
		}

		if (mainHandItem.getItem() instanceof BackpackItem || !isTool(mainHandItem)) {
			return false;
		}

		if (filterLogic.isAllowList()) {
			BackpackInventoryHandler backpackInventory = backpackWrapper.getInventoryHandler();
			AtomicBoolean result = new AtomicBoolean(false);
			InventoryHelper.iterate(backpackInventory, (slot, stack) -> {
				if (filterLogic.matchesWeaponFilter(stack)) {
					result.set(swapWeapon(player, mainHandItem, backpackInventory, stack));
				}
			});
			return result.get();
		}

		return tryToSwapInWeapon(player, mainHandItem);
	}

	private boolean isTool(ItemStack stack) {
		return !stack.getToolTypes().isEmpty() || !ToolRegistry.getItemToolTypes(stack).isEmpty() || stack.getItem() instanceof ShearsItem || stack.getItem().is(Tags.Items.SHEARS);
	}

	private boolean isSword(ItemStack stack, PlayerEntity player) {
		if (SwordRegistry.isSword(stack)) {
			return true;
		}

		ModifiableAttributeInstance attackDamage = player.getAttribute(Attributes.ATTACK_DAMAGE);
		if (!stack.isEmpty() && hasSwordOrNoToolTypes(stack)) {
			return attackDamage != null && attackDamage.getModifier(Item.BASE_ATTACK_DAMAGE_UUID) != null;
		}
		return false;
	}

	private boolean tryToSwapInWeapon(PlayerEntity player, ItemStack mainHandItem) {
		AtomicReference<ItemStack> bestAxe = new AtomicReference<>(ItemStack.EMPTY);
		AtomicDouble bestAxeDamage = new AtomicDouble(0);
		AtomicReference<ItemStack> bestSword = new AtomicReference<>(ItemStack.EMPTY);
		AtomicDouble bestSwordDamage = new AtomicDouble(0);

		updateBestWeapons(bestAxe, bestAxeDamage, bestSword, bestSwordDamage, mainHandItem);

		IItemHandlerModifiable backpackInventory = backpackWrapper.getInventoryHandler();
		InventoryHelper.iterate(backpackInventory, (slot, stack) -> {
			if (filterLogic.matchesWeaponFilter(stack)) {
				updateBestWeapons(bestAxe, bestAxeDamage, bestSword, bestSwordDamage, stack);
			}
		});

		if (!bestSword.get().isEmpty()) {
			return swapWeapon(player, mainHandItem, backpackInventory, bestSword.get());
		} else if (!bestAxe.get().isEmpty()) {
			return swapWeapon(player, mainHandItem, backpackInventory, bestAxe.get());
		}
		return false;
	}

	private void updateBestWeapons(AtomicReference<ItemStack> bestAxe, AtomicDouble bestAxeDamage, AtomicReference<ItemStack> bestSword, AtomicDouble bestSwordDamage, ItemStack stack) {
		ModifiableAttributeInstance attribute = new ModifiableAttributeInstance(Attributes.ATTACK_DAMAGE, a -> {});
		Multimap<Attribute, AttributeModifier> attributeModifiers = stack.getAttributeModifiers(EquipmentSlotType.MAINHAND);
		if (!attributeModifiers.containsKey(Attributes.ATTACK_DAMAGE)) {
			return;
		}
		attributeModifiers.get(Attributes.ATTACK_DAMAGE).forEach(m -> {
			attribute.removeModifier(m);
			attribute.addTransientModifier(m);
		});
		double damageValue = attribute.getValue();
		if (getToolTypes(stack).contains(ToolType.AXE)) {
			if (damageValue > bestAxeDamage.get()) {
				bestAxe.set(stack);
				bestAxeDamage.set(damageValue);
			}
		} else if ((SwordRegistry.isSword(stack) || hasSwordOrNoToolTypes(stack)) && damageValue > bestSwordDamage.get()) {
			bestSword.set(stack);
			bestSwordDamage.set(damageValue);
		}
	}

	private boolean hasSwordOrNoToolTypes(ItemStack stack) {
		Set<ToolType> toolTypes = getToolTypes(stack);
		return toolTypes.isEmpty() || toolTypes.contains(SWORD_TOOL_TYPE);
	}

	private boolean swapWeapon(PlayerEntity player, ItemStack mainHandItem, IItemHandlerModifiable backpackInventory, ItemStack sword) {
		if (sword == mainHandItem) {
			return true;
		}

		InventoryHelper.extractFromInventory(sword, backpackInventory, false);
		if (InventoryHelper.insertIntoInventory(mainHandItem, backpackInventory, true).isEmpty()) {
			player.setItemInHand(Hand.MAIN_HAND, sword);
			InventoryHelper.insertIntoInventory(mainHandItem, backpackInventory, false);
			return true;
		} else {
			InventoryHelper.insertIntoInventory(sword, backpackInventory, false);
			return false;
		}
	}

	@Override
	public boolean hideSettingsTab() {
		return !upgradeItem.hasSettingsTab();
	}

	public ToolSwapperFilterLogic getFilterLogic() {
		return filterLogic;
	}

	public boolean shouldSwapWeapon() {
		return NBTHelper.getBoolean(upgrade, "shouldSwapWeapon").orElse(true);
	}

	public void setSwapWeapon(boolean shouldSwapWeapon) {
		NBTHelper.setBoolean(upgrade, "shouldSwapWeapon", shouldSwapWeapon);
		save();
	}

	public ToolSwapMode getToolSwapMode() {
		return NBTHelper.getEnumConstant(upgrade, "toolSwapMode", ToolSwapMode::fromName).orElse(ToolSwapMode.ANY);
	}

	public void setToolSwapMode(ToolSwapMode toolSwapMode) {
		NBTHelper.setEnumConstant(upgrade, "toolSwapMode", toolSwapMode);
		save();
	}

	@Override
	public boolean onEntityInteract(World world, Entity entity, PlayerEntity player) {
		if (!upgradeItem.shouldSwapToolOnKeyPress()) {
			return false;
		}

		return tryToSwapTool(player, stack -> itemWorksOnEntity(stack, entity), entity.getType().getRegistryName());
	}

	private boolean itemWorksOnEntity(ItemStack stack, Entity entity) {
		if (isShearableEntity(entity, stack) && isShearsItem(stack)) {
			return true;
		}
		return ToolRegistry.isToolForEntity(stack, entity);
	}

	@Override
	public boolean onBlockInteract(World world, BlockPos pos, BlockState blockState, PlayerEntity player) {
		if (!upgradeItem.shouldSwapToolOnKeyPress()) {
			return false;
		}

		return tryToSwapTool(player, stack -> itemWorksOnBlock(world, pos, blockState, player, stack), blockState.getBlock().getRegistryName());
	}

	private boolean tryToSwapTool(PlayerEntity player, Predicate<ItemStack> isToolValid, @Nullable ResourceLocation targetRegistryName) {
		ItemStack mainHandStack = player.getMainHandItem();
		if (mainHandStack.getItem() instanceof BackpackItem) {
			return false;
		}
		if (toolCacheFor == null || !toolCacheFor.equals(targetRegistryName)) {
			toolCache.clear();
			toolCacheFor = targetRegistryName;
		}

		boolean itemInHandIsValid = isToolValid.test(mainHandStack);

		BackpackInventoryHandler backpackInventory = backpackWrapper.getInventoryHandler();
		if (itemInHandIsValid && toolCache.stream().noneMatch(st -> ItemStack.isSameIgnoreDurability(st, mainHandStack))) {
			toolCache.offer(mainHandStack);
		}
		ItemStack tool = findToolToSwap(backpackInventory, isToolValid);

		if (tool.isEmpty()) {
			return false;
		}

		tool = tool.copy().split(1);

		if ((tool.getCount() == 1 || InventoryHelper.insertIntoInventory(mainHandStack, backpackInventory, true).isEmpty())) {
			player.setItemInHand(Hand.MAIN_HAND, InventoryHelper.extractFromInventory(tool, backpackInventory, false));
			InventoryHelper.insertIntoInventory(mainHandStack, backpackInventory, false);
			toolCache.offer(tool);
		}
		return true;
	}

	private ItemStack findToolToSwap(BackpackInventoryHandler backpackInventory, Predicate<ItemStack> isValidTool) {
		Set<ItemStack> alreadyGivenBefore = new HashSet<>();
		AtomicReference<ItemStack> toolFound = new AtomicReference<>(ItemStack.EMPTY);
		InventoryHelper.iterate(backpackInventory, (slot, stack) -> {
			if (stack.isEmpty()) {
				return;
			}

			if (!hasEquivalentItem(toolCache, stack)) {
				if (isValidTool.test(stack)) {
					toolFound.set(stack);
				}
			} else {
				alreadyGivenBefore.add(stack);
			}
		}, () -> !toolFound.get().isEmpty());

		if (toolFound.get().isEmpty() && !alreadyGivenBefore.isEmpty()) {
			while (toolCache.peek() != null) {
				ItemStack itemStack = toolCache.poll();
				if (hasEquivalentItem(alreadyGivenBefore, itemStack)) {
					toolFound.set(itemStack);
					break;
				}
			}
		}
		return toolFound.get();
	}

	private boolean hasEquivalentItem(Collection<ItemStack> alreadyGivenBefore, ItemStack stack) {
		for (ItemStack givenTool : alreadyGivenBefore) {
			if (ItemStack.isSameIgnoreDurability(givenTool, stack)) {
				return true;
			}
		}

		return false;
	}

	private boolean itemWorksOnBlock(World world, BlockPos pos, BlockState blockState, PlayerEntity player, ItemStack stack) {
		for (ToolType toolType : getToolTypes(stack)) {
			if (blockState.getToolModifiedState(world, pos, player, stack, toolType) != null) {
				return true;
			}
		}

		Block block = blockState.getBlock();
		if (isShearInteractionBlock(world, pos, stack, block) && isShearsItem(stack)) {
			return true;
		}

		return ToolRegistry.isToolForBlock(stack, block, world, blockState, pos);
	}

	private boolean isShearsItem(ItemStack stack) {
		return stack.getItem() instanceof ShearsItem || stack.getItem().is(Tags.Items.SHEARS);
	}

	private boolean isShearInteractionBlock(World world, BlockPos pos, ItemStack stack, Block block) {
		return (block instanceof IForgeShearable && ((IForgeShearable) block).isShearable(stack, world, pos)) || block instanceof BeehiveBlock;
	}

	private boolean isShearableEntity(Entity entity, ItemStack stack) {
		return entity instanceof IForgeShearable && ((IForgeShearable) entity).isShearable(stack, entity.level, entity.blockPosition());
	}

	@Override
	public boolean canProcessBlockInteract() {
		return upgradeItem.shouldSwapToolOnKeyPress();
	}

	@Override
	public boolean canProcessEntityInteract() {
		return upgradeItem.shouldSwapToolOnKeyPress();
	}
}
