package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import com.google.common.util.concurrent.AtomicDouble;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.ai.attributes.AttributeInstance;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.ShearsItem;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.BeehiveBlock;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.Vec3;
import net.neoforged.neoforge.common.IShearable;
import net.neoforged.neoforge.common.ItemAbilities;
import net.neoforged.neoforge.common.ItemAbility;
import net.neoforged.neoforge.common.Tags;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IAttackEntityResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBlockClickResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBlockToolSwapUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IEntityToolSwapUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModDataComponents;
import net.p3pp3rf1y.sophisticatedbackpacks.registry.tool.SwordRegistry;
import net.p3pp3rf1y.sophisticatedbackpacks.registry.tool.ToolRegistry;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.inventory.IItemHandlerSimpleInserter;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedcore.util.CoreFakePlayer;
import net.p3pp3rf1y.sophisticatedcore.util.InventoryHelper;

import javax.annotation.Nullable;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.Predicate;

import static net.neoforged.neoforge.common.ItemAbilities.*;

public class ToolSwapperUpgradeWrapper extends UpgradeWrapperBase<ToolSwapperUpgradeWrapper, ToolSwapperUpgradeItem>
		implements IBlockClickResponseUpgrade, IAttackEntityResponseUpgrade, IBlockToolSwapUpgrade, IEntityToolSwapUpgrade {

	private static final LoadingCache<ItemStack, Boolean> isToolCache = CacheBuilder.newBuilder().expireAfterAccess(1, TimeUnit.MINUTES).build(
			new CacheLoader<>() {
				@Override
				public Boolean load(ItemStack key) {
					return canPerformToolAction(key);
				}
			}
	);

	private final FilterLogic filterLogic;
	@Nullable
	private ResourceLocation toolCacheFor = null;
	private final Queue<ItemStack> toolCache = new LinkedList<>();

	private Block lastMinedBlock = Blocks.AIR;

	protected ToolSwapperUpgradeWrapper(IStorageWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
		filterLogic = new FilterLogic(upgrade, upgradeSaveHandler, Config.SERVER.toolSwapperUpgrade.filterSlots.get(), ModCoreDataComponents.FILTER_ATTRIBUTES);
	}

	@Override
	public boolean onBlockClick(Player player, BlockPos pos) {
		ToolSwapMode toolSwapMode = getToolSwapMode();
		if (player.isCreative() || player.isSpectator() || toolSwapMode == ToolSwapMode.NO_SWAP) {
			return false;
		}

		BlockState state = player.level().getBlockState(pos);
		Block block = state.getBlock();

		if (state.isAir()) {
			return false;
		}

		ItemStack mainHandItem = player.getMainHandItem();
		if (mainHandItem.getItem() instanceof BackpackItem || (toolSwapMode == ToolSwapMode.ONLY_TOOLS && isSword(mainHandItem, player)) || (!isSword(mainHandItem, player) && isNotTool(mainHandItem)) || !filterLogic.matchesFilter(mainHandItem)) {
			return false;
		}

		double mainToolSpeed = 0;
		if (isGoodAtBreakingBlock(player, pos, state, mainHandItem)) {
			if (lastMinedBlock == block || state.getDestroySpeed(player.level(), pos) == 0) {
				return true;
			}
			mainToolSpeed = mainHandItem.getDestroySpeed(state);
		}

		lastMinedBlock = block;

		return tryToSwapTool(player, pos, state, mainToolSpeed, mainHandItem);
	}

	private boolean tryToSwapTool(Player player, BlockPos pos, BlockState state, double mainHandItemSpeed, ItemStack mainHandItem) {
		if (!(player.level() instanceof ServerLevel serverLevel)) {
			return false;
		}
		CoreFakePlayer fakePlayer = CoreFakePlayer.get(serverLevel);
		fakePlayer.setPosition(Vec3.atCenterOf(pos.above()));
		AtomicReference<ItemStack> selectedTool = new AtomicReference<>(ItemStack.EMPTY);
		AtomicInteger selectedSlot = new AtomicInteger(-1);
		AtomicDouble bestSpeed = new AtomicDouble(mainHandItemSpeed);
		AtomicBoolean instantDestroyProgress = new AtomicBoolean(false);
		IItemHandlerSimpleInserter backpackInventory = storageWrapper.getInventoryHandler();
		InventoryHelper.iterate(backpackInventory, (slot, stack) -> {
			if (stack.isEmpty()) {
				return;
			}
			if (isAllowedAndGoodAtBreakingBlock(fakePlayer, pos, state, stack)) {
				if (state.getDestroyProgress(fakePlayer, serverLevel, pos) >= 1.0f) {
					selectedSlot.set(slot);
					selectedTool.set(stack);
					instantDestroyProgress.set(true);
				} else {
					float destroySpeed = stack.getDestroySpeed(state);
					if (bestSpeed.get() < destroySpeed) {
						bestSpeed.set(destroySpeed);
						selectedSlot.set(slot);
						selectedTool.set(stack);
					}
				}
			}
		}, instantDestroyProgress::get);
		fakePlayer.setItemInHand(InteractionHand.MAIN_HAND, ItemStack.EMPTY);

		ItemStack tool = selectedTool.get();
		if (!tool.isEmpty() && hasSpaceInBackpackOrCanPlaceInTheSlotOfSwappedTool(backpackInventory, mainHandItem, tool, selectedSlot.get())) {
			player.setItemInHand(InteractionHand.MAIN_HAND, backpackInventory.extractItem(selectedSlot.get(), 1, false));
			backpackInventory.insertItem(mainHandItem, false);
			return true;
		}

		return false;
	}

	private boolean hasSpaceInBackpackOrCanPlaceInTheSlotOfSwappedTool(IItemHandlerSimpleInserter backpackInventory, ItemStack mainHandItem, ItemStack tool, int selectedSlot) {
		return (backpackInventory.insertItem(mainHandItem, true).isEmpty())
				|| (tool.getCount() == 1 && backpackInventory.isItemValid(selectedSlot, mainHandItem));
	}

	private boolean isAllowedAndGoodAtBreakingBlock(Player player, BlockPos pos, BlockState state, ItemStack stack) {
		if (!filterLogic.matchesFilter(stack)) {
			return false;
		}

		player.setItemInHand(InteractionHand.MAIN_HAND, stack);

		return isGoodAtBreakingBlock(player, pos, state, stack);
	}

	private boolean isGoodAtBreakingBlock(Player player, BlockPos pos, BlockState state, ItemStack stack) {
		return (!state.requiresCorrectToolForDrops() || stack.isCorrectToolForDrops(state)) && (stack.getDestroySpeed(state) > 1.5 || state.getDestroyProgress(player, player.level(), pos) >= 1.0f);
	}

	@Override
	public boolean onAttackEntity(Player player) {
		if (!shouldSwapWeapon()) {
			return false;
		}

		ItemStack mainHandItem = player.getMainHandItem();

		if (isSword(mainHandItem, player)) {
			return true;
		}

		if (mainHandItem.getItem() instanceof BackpackItem || isNotTool(mainHandItem) || !filterLogic.matchesFilter(mainHandItem)) {
			return false;
		}

		return tryToSwapInWeapon(player, mainHandItem);
	}

	private boolean isNotTool(ItemStack stack) {
		return !isToolCache.getUnchecked(stack);
	}

	private static boolean canPerformToolAction(ItemStack stack) {
		return canPerformAnyAction(stack, ItemAbilities.DEFAULT_AXE_ACTIONS) || canPerformAnyAction(stack, ItemAbilities.DEFAULT_HOE_ACTIONS)
				|| canPerformAnyAction(stack, ItemAbilities.DEFAULT_PICKAXE_ACTIONS) || canPerformAnyAction(stack, ItemAbilities.DEFAULT_SHOVEL_ACTIONS)
				|| canPerformAnyAction(stack, ItemAbilities.DEFAULT_SHEARS_ACTIONS);
	}

	private static boolean canPerformAnyAction(ItemStack stack, Set<ItemAbility> toolActions) {
		for (ItemAbility toolAction : toolActions) {
			if (stack.canPerformAction(toolAction)) {
				return true;
			}
		}
		return false;
	}

	private boolean isSword(ItemStack stack, Player player) {
		if (SwordRegistry.isSword(stack)) {
			return true;
		}

		AttributeInstance attackDamage = player.getAttribute(Attributes.ATTACK_DAMAGE);
		if (!stack.isEmpty() && stack.canPerformAction(ItemAbilities.SWORD_SWEEP)) {
			return attackDamage != null && attackDamage.getModifier(Item.BASE_ATTACK_DAMAGE_ID) != null;
		}
		return false;
	}

	private boolean tryToSwapInWeapon(Player player, ItemStack mainHandItem) {
		AtomicReference<ItemStack> bestAxe = new AtomicReference<>(ItemStack.EMPTY);
		AtomicDouble bestAxeDamage = new AtomicDouble(0);
		AtomicReference<ItemStack> bestSword = new AtomicReference<>(ItemStack.EMPTY);
		AtomicDouble bestSwordDamage = new AtomicDouble(0);

		updateBestWeapons(bestAxe, bestAxeDamage, bestSword, bestSwordDamage, mainHandItem);

		IItemHandlerSimpleInserter backpackInventory = storageWrapper.getInventoryForUpgradeProcessing();
		InventoryHelper.iterate(backpackInventory, (slot, stack) -> {
			if (filterLogic.matchesFilter(stack)) {
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
		AttributeInstance attribute = new AttributeInstance(Attributes.ATTACK_DAMAGE, a -> {
		});
		stack.forEachModifier(EquipmentSlot.MAINHAND, (att, m) -> {
			if (!att.equals(Attributes.ATTACK_DAMAGE)) {
				return;
			}
			attribute.removeModifier(m);
			attribute.addTransientModifier(m);
		});

		double damageValue = attribute.getValue();
		if (stack.canPerformAction(ItemAbilities.AXE_DIG)) {
			if (damageValue > bestAxeDamage.get()) {
				bestAxe.set(stack);
				bestAxeDamage.set(damageValue);
			}
		} else if ((SwordRegistry.isSword(stack) || stack.canPerformAction(ItemAbilities.SWORD_SWEEP)) && damageValue > bestSwordDamage.get()) {
			bestSword.set(stack);
			bestSwordDamage.set(damageValue);
		}
	}

	private boolean swapWeapon(Player player, ItemStack mainHandItem, IItemHandlerSimpleInserter backpackInventory, ItemStack sword) {
		if (sword == mainHandItem) {
			return true;
		}

		ItemStack swordCopy = sword.copy();
		swordCopy.setCount(1);
		InventoryHelper.extractFromInventory(swordCopy, backpackInventory, false);
		if (backpackInventory.insertItem(mainHandItem, true).isEmpty()) {
			player.setItemInHand(InteractionHand.MAIN_HAND, swordCopy);
			backpackInventory.insertItem(mainHandItem, false);
			return true;
		} else {
			backpackInventory.insertItem(swordCopy, false);
			return false;
		}
	}

	@Override
	public boolean hideSettingsTab() {
		return !upgradeItem.hasSettingsTab();
	}

	public FilterLogic getFilterLogic() {
		return filterLogic;
	}

	public boolean shouldSwapWeapon() {
		return upgrade.getOrDefault(ModDataComponents.SHOULD_SWAP_WEAPON, true);
	}

	public void setSwapWeapon(boolean shouldSwapWeapon) {
		upgrade.set(ModDataComponents.SHOULD_SWAP_WEAPON, shouldSwapWeapon);
		save();
	}

	public ToolSwapMode getToolSwapMode() {
		return upgrade.getOrDefault(ModDataComponents.TOOL_SWAP_MODE, ToolSwapMode.ANY);
	}

	public void setToolSwapMode(ToolSwapMode toolSwapMode) {
		upgrade.set(ModDataComponents.TOOL_SWAP_MODE, toolSwapMode);
		save();
	}

	@Override
	public boolean onEntityInteract(Level level, Entity entity, Player player) {
		if (!upgradeItem.shouldSwapToolOnKeyPress()) {
			return false;
		}

		return tryToSwapTool(player, stack -> itemWorksOnEntity(stack, entity), BuiltInRegistries.ENTITY_TYPE.getKey(entity.getType()));
	}

	private boolean itemWorksOnEntity(ItemStack stack, Entity entity) {
		if (isShearableEntity(entity, stack) && isShearsItem(stack)) {
			return true;
		}
		return ToolRegistry.isToolForEntity(stack, entity);
	}

	@Override
	public boolean onBlockInteract(Level level, BlockPos pos, BlockState blockState, Player player) {
		if (!upgradeItem.shouldSwapToolOnKeyPress()) {
			return false;
		}

		return tryToSwapTool(player, stack -> itemWorksOnBlock(level, pos, blockState, player, stack), BuiltInRegistries.BLOCK.getKey(blockState.getBlock()));
	}

	private boolean tryToSwapTool(Player player, Predicate<ItemStack> isToolValid, @Nullable ResourceLocation targetRegistryName) {
		ItemStack mainHandStack = player.getMainHandItem();
		if (mainHandStack.getItem() instanceof BackpackItem) {
			return false;
		}
		if (toolCacheFor == null || !toolCacheFor.equals(targetRegistryName)) {
			toolCache.clear();
			toolCacheFor = targetRegistryName;
		}

		boolean itemInHandIsValid = isToolValid.test(mainHandStack);

		IItemHandlerSimpleInserter backpackInventory = storageWrapper.getInventoryForUpgradeProcessing();
		if (itemInHandIsValid && toolCache.stream().noneMatch(st -> ItemStack.isSameItem(st, mainHandStack))) {
			toolCache.offer(mainHandStack);
		}
		ItemStack tool = findToolToSwap(backpackInventory, isToolValid);

		if (tool.isEmpty()) {
			return false;
		}

		tool = tool.copy().split(1);

		if ((tool.getCount() == 1 || backpackInventory.insertItem(mainHandStack, true).isEmpty())) {
			player.setItemInHand(InteractionHand.MAIN_HAND, InventoryHelper.extractFromInventory(tool, backpackInventory, false));
			backpackInventory.insertItem(mainHandStack, false);
			toolCache.offer(tool);
		}
		return true;
	}

	private ItemStack findToolToSwap(IItemHandlerSimpleInserter backpackInventory, Predicate<ItemStack> isValidTool) {
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
			if (ItemStack.isSameItem(givenTool, stack)) {
				return true;
			}
		}

		return false;
	}

	private static final Set<ItemAbility> BLOCK_MODIFICATION_ACTIONS = Set.of(AXE_STRIP, AXE_SCRAPE, AXE_WAX_OFF, SHOVEL_FLATTEN, SHEARS_CARVE, SHEARS_HARVEST);

	private boolean itemWorksOnBlock(Level level, BlockPos pos, BlockState blockState, Player player, ItemStack stack) {
		for (ItemAbility action : BLOCK_MODIFICATION_ACTIONS) {
			if (stack.canPerformAction(action) && blockState.getToolModifiedState(
					new UseOnContext(level, player, InteractionHand.MAIN_HAND, stack, new BlockHitResult(Vec3.atCenterOf(pos), Direction.UP, pos, true)), action, true) != null) {
				return true;
			}
		}
		Block block = blockState.getBlock();
		if (isShearInteractionBlock(level, pos, stack, block) && isShearsItem(stack)) {
			return true;
		}

		return ToolRegistry.isToolForBlock(stack, block, level, blockState, pos);
	}

	private boolean isShearsItem(ItemStack stack) {
		return stack.getItem() instanceof ShearsItem || stack.is(Tags.Items.TOOLS_SHEAR);
	}

	private boolean isShearInteractionBlock(Level level, BlockPos pos, ItemStack stack, Block block) {
		return (block instanceof IShearable shearable && shearable.isShearable(null, stack, level, pos)) || block instanceof BeehiveBlock;
	}

	private boolean isShearableEntity(Entity entity, ItemStack stack) {
		return entity instanceof IShearable shearable && shearable.isShearable(null, stack, entity.level(), entity.blockPosition());
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
