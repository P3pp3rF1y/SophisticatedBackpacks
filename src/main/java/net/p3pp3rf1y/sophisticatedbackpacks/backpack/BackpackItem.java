package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.ChatFormatting;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.screens.inventory.CreativeModeInventoryScreen;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.component.DataComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.sounds.SoundSource;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.SimpleMenuProvider;
import net.minecraft.world.entity.*;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.ClickAction;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.inventory.tooltip.TooltipComponent;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.item.component.TooltipDisplay;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.item.equipment.ArmorMaterials;
import net.minecraft.world.item.equipment.Equippable;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.SoundType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.material.FluidState;
import net.minecraft.world.level.material.Fluids;
import net.minecraft.world.phys.Vec3;
import net.minecraft.world.phys.shapes.CollisionContext;
import net.neoforged.fml.loading.FMLEnvironment;
import net.neoforged.neoforge.transfer.item.ItemResource;
import net.neoforged.neoforge.transfer.transaction.Transaction;
import net.neoforged.neoforge.transfer.transaction.TransactionContext;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContext;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.everlasting.EverlastingBackpackItemEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.everlasting.EverlastingUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryInteractionHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.api.IStashStorageItem;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.TranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.settings.memory.MemorySettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.ServerStorageSoundHandler;
import net.p3pp3rf1y.sophisticatedcore.util.ColorHelper;
import net.p3pp3rf1y.sophisticatedcore.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedcore.util.ItemBase;
import net.p3pp3rf1y.sophisticatedcore.util.WorldHelper;

import javax.annotation.Nullable;

import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.IntSupplier;
import java.util.function.Supplier;
import java.util.function.UnaryOperator;

import static net.minecraft.world.level.block.state.properties.BlockStateProperties.WATERLOGGED;

public class BackpackItem extends ItemBase implements IStashStorageItem {
	private final IntSupplier numberOfSlots;
	private final IntSupplier numberOfUpgradeSlots;
	private final Supplier<BackpackBlock> blockSupplier;

	public BackpackItem(IntSupplier numberOfSlots, IntSupplier numberOfUpgradeSlots, Supplier<BackpackBlock> blockSupplier, Properties properties) {
		this(numberOfSlots, numberOfUpgradeSlots, blockSupplier, p -> p, properties);
	}

	public BackpackItem(IntSupplier numberOfSlots, IntSupplier numberOfUpgradeSlots, Supplier<BackpackBlock> blockSupplier,
			UnaryOperator<Properties> updateProperties, Properties properties) {
		super(updateProperties.apply(properties.stacksTo(1).component(DataComponents.EQUIPPABLE,
				Equippable.builder(EquipmentSlot.CHEST).setEquipSound(ArmorMaterials.LEATHER.equipSound()).build())));
		this.numberOfSlots = numberOfSlots;
		this.numberOfUpgradeSlots = numberOfUpgradeSlots;
		this.blockSupplier = blockSupplier;
	}

	public Block getBackpackBlock() {
		return blockSupplier.get();
	}

	public static void setColors(ItemStack backpackStack, int mainColor, int accentColor) {
		backpackStack.set(ModCoreDataComponents.MAIN_COLOR, mainColor);
		backpackStack.set(ModCoreDataComponents.ACCENT_COLOR, accentColor);
	}

	@Override
	public void addCreativeTabItems(Consumer<ItemStack> itemConsumer) {
		super.addCreativeTabItems(itemConsumer);

		if (this != ModItems.BACKPACK.get() || !net.p3pp3rf1y.sophisticatedcore.Config.COMMON.enabledItems.isItemEnabled(this)) {
			return;
		}

		for (DyeColor color : DyeColor.values()) {
			ItemStack stack = new ItemStack(this);
			setColors(stack, color.getTextureDiffuseColor(), color.getTextureDiffuseColor());
			itemConsumer.accept(stack);
		}

		int mainColor = ColorHelper.calculateColor(BackpackWrapper.DEFAULT_MAIN_COLOR, BackpackWrapper.DEFAULT_MAIN_COLOR,
				List.of(DyeColor.YELLOW, DyeColor.LIME));
		int accentColor = ColorHelper.calculateColor(BackpackWrapper.DEFAULT_ACCENT_COLOR, BackpackWrapper.DEFAULT_ACCENT_COLOR,
				List.of(DyeColor.BLUE, DyeColor.BLACK));

		ItemStack stack = new ItemStack(this);
		setColors(stack, mainColor, accentColor);
		itemConsumer.accept(stack);
	}

	@Override
	public void appendHoverText(ItemStack stack, TooltipContext context, TooltipDisplay tooltipDisplay, Consumer<Component> tooltipAdder,
			TooltipFlag tooltipFlag) {
		super.appendHoverText(stack, context, tooltipDisplay, tooltipAdder, tooltipFlag);
		if (tooltipFlag.isAdvanced()) {
			BackpackWrapper.fromStack(stack).getContentsUuid()
					.ifPresent(uuid -> tooltipAdder.accept(Component.literal("UUID: " + uuid).withStyle(ChatFormatting.DARK_GRAY)));
		}
		if (!Minecraft.getInstance().hasShiftDown()) {
			tooltipAdder.accept(Component
					.translatable(TranslationHelper.INSTANCE.translItemTooltip("storage") + ".press_for_contents",
							Component.translatable(TranslationHelper.INSTANCE.translItemTooltip("storage") + ".shift").withStyle(ChatFormatting.AQUA))
					.withStyle(ChatFormatting.GRAY));
		}
	}

	@Override
	public Optional<TooltipComponent> getTooltipImage(ItemStack stack) {
		if (FMLEnvironment.getDist().isClient()) {
			return Optional.ofNullable(BackpackItemClient.getTooltipImage(stack));
		}
		return Optional.empty();
	}

	@Override
	public boolean hasCustomEntity(ItemStack stack) {
		return true;
	}

	private boolean hasEverlastingUpgrade(ItemStack stack) {
		return !BackpackWrapper.fromStack(stack).getUpgradeHandler().getTypeWrappers(EverlastingUpgradeItem.TYPE).isEmpty();
	}

	@Nullable
	@Override
	public Entity createEntity(Level level, Entity entity, ItemStack itemstack) {
		if (!(entity instanceof ItemEntity itemEntity)) {
			return null;
		}

		UUIDDeduplicator.dedupeBackpackItemEntityInArea(itemEntity);

		return hasEverlastingUpgrade(itemstack) ? createEverlastingBackpack(level, (ItemEntity) entity, itemstack) : null;
	}

	@Nullable
	private EverlastingBackpackItemEntity createEverlastingBackpack(Level level, ItemEntity itemEntity, ItemStack itemstack) {
		EverlastingBackpackItemEntity backpackItemEntity = ModItems.EVERLASTING_BACKPACK_ITEM_ENTITY.get().create(level, EntitySpawnReason.EVENT);
		if (backpackItemEntity != null) {
			backpackItemEntity.setPos(itemEntity.getX(), itemEntity.getY(), itemEntity.getZ());
			backpackItemEntity.setItem(itemstack);
			backpackItemEntity.setPickUpDelay(itemEntity.pickupDelay);
			if (itemEntity.getOwner() != null) {
				backpackItemEntity.setThrower(itemEntity.getOwner());
			}
			backpackItemEntity.setDeltaMovement(itemEntity.getDeltaMovement());
		}
		return backpackItemEntity;
	}

	@Override
	public InteractionResult useOn(UseOnContext context) {
		Player player = context.getPlayer();
		if (player == null || !player.isShiftKeyDown()) {
			return InteractionResult.PASS;
		}

		if (InventoryInteractionHelper.tryInventoryInteraction(context)) {
			return InteractionResult.SUCCESS;
		}

		Direction direction = player.getDirection().getOpposite();

		BlockPlaceContext blockItemUseContext = new BlockPlaceContext(context);
		InteractionResult result = tryPlace(player, direction, blockItemUseContext);
		return result == InteractionResult.PASS ? super.useOn(context) : result;
	}

	public InteractionResult tryPlace(@Nullable Player player, Direction direction, BlockPlaceContext blockItemUseContext) {
		if (!blockItemUseContext.canPlace()) {
			return InteractionResult.FAIL;
		}
		Level level = blockItemUseContext.getLevel();
		BlockPos pos = blockItemUseContext.getClickedPos();

		FluidState fluidstate = blockItemUseContext.getLevel().getFluidState(pos);
		BlockState placementState = blockSupplier.get().defaultBlockState().setValue(BackpackBlock.FACING, direction).setValue(WATERLOGGED,
				fluidstate.getType() == Fluids.WATER);
		if (!canPlace(blockItemUseContext, placementState)) {
			return InteractionResult.FAIL;
		}

		if (level.setBlockAndUpdate(pos, placementState)) {
			ItemStack backpack = blockItemUseContext.getItemInHand();
			WorldHelper.getBlockEntity(level, pos, BackpackBlockEntity.class).ifPresent(be -> {
				be.setBackpack(getBackpackCopy(player, backpack));
				be.refreshRenderState();

				be.tryToAddToController();
			});

			if (!level.isClientSide()) {
				stopBackpackSounds(backpack, level, pos);
			}

			SoundType soundtype = placementState.getSoundType(level, pos, player);
			level.playSound(player, pos, soundtype.getPlaceSound(), SoundSource.BLOCKS, (soundtype.getVolume() + 1.0F) / 2.0F, soundtype.getPitch() * 0.8F);
			if (player == null || !player.isCreative()) {
				backpack.shrink(1);
			}

			return InteractionResult.SUCCESS;
		}
		return InteractionResult.PASS;
	}

	private static void stopBackpackSounds(ItemStack backpack, Level level, BlockPos pos) {
		BackpackWrapper.fromStack(backpack).getContentsUuid().ifPresent(uuid -> ServerStorageSoundHandler.stopPlayingDisc(level, Vec3.atCenterOf(pos), uuid));
	}

	private ItemStack getBackpackCopy(@Nullable Player player, ItemStack backpack) {
		if (player == null || !player.isCreative()) {
			return backpack.copy();
		}
		return BackpackWrapper.fromStack(backpack).cloneBackpack();
	}

	protected boolean canPlace(BlockPlaceContext context, BlockState state) {
		Player playerentity = context.getPlayer();
		CollisionContext iselectioncontext = playerentity == null ? CollisionContext.empty() : CollisionContext.of(playerentity);
		return (state.canSurvive(context.getLevel(), context.getClickedPos()))
				&& context.getLevel().isUnobstructed(state, context.getClickedPos(), iselectioncontext);
	}

	@Override
	public InteractionResult use(Level level, Player player, InteractionHand hand) {
		ItemStack stack = player.getItemInHand(hand);

		if (!level.isClientSide()) {
			String handlerName = hand == InteractionHand.MAIN_HAND ? PlayerInventoryProvider.MAIN_INVENTORY : PlayerInventoryProvider.OFFHAND_INVENTORY;
			int slot = hand == InteractionHand.MAIN_HAND ? player.getInventory().getSelectedSlot() : 0;
			BackpackContext.Item context = new BackpackContext.Item(handlerName, slot);
			player.openMenu(new SimpleMenuProvider((w, p, pl) -> new BackpackContainer(w, pl, context), stack.getHoverName()), context::toBuffer);
		}
		return InteractionResult.SUCCESS.heldItemTransformedTo(stack);
	}

	@Override
	public void inventoryTick(ItemStack stack, ServerLevel level, Entity entity, @Nullable EquipmentSlot slot) {
		if (level.isClientSide() || !(entity instanceof Player player) || player.isSpectator() || player.isDeadOrDying()
				|| (Config.SERVER.nerfsConfig.onlyWornBackpackTriggersUpgrades.get() && slot == null)) {
			return;
		}
		IBackpackWrapper backpackWrapper = BackpackWrapper.fromStack(stack);
		backpackWrapper.getUpgradeHandler().getWrappersThatImplement(ITickableUpgrade.class).forEach(upgrade -> {
			if (level.isClientSide()) {
				upgrade.clientTick(player, player.level(), player.blockPosition());
			} else {
				upgrade.tick(player, player.level(), player.blockPosition());
			}
		});
		super.inventoryTick(stack, level, entity, slot);
	}

	public int getNumberOfSlots() {
		return numberOfSlots.getAsInt();
	}

	public int getNumberOfUpgradeSlots() {
		return numberOfUpgradeSlots.getAsInt();
	}

	@Override
	public boolean onDroppedByPlayer(ItemStack item, Player player) {
		return !(player.containerMenu instanceof BackpackContainer backpackContainer
				&& backpackContainer.getVisibleStorageItem().map(visibleStorageItem -> visibleStorageItem == item).orElse(false));
	}

	@Nullable
	@Override
	public EquipmentSlot getEquipmentSlot(ItemStack stack) {
		return EquipmentSlot.CHEST;
	}

	@Override
	public boolean shouldCauseReequipAnimation(ItemStack oldStack, ItemStack newStack, boolean slotChanged) {
		return slotChanged;
	}

	@Override
	public boolean makesPiglinsNeutral(ItemStack stack, LivingEntity wearer) {
		return stack.getItem() == ModItems.GOLD_BACKPACK.get();
	}

	@Override
	public Optional<TooltipComponent> getInventoryTooltip(ItemStack stack) {
		return Optional.of(new BackpackContentsTooltip(stack));
	}

	public int stash(ItemStack storageStack, ItemResource resource, int amount, TransactionContext tx) {
		return BackpackWrapper.fromStack(storageStack).getInventoryForUpgradeProcessing().insert(resource, amount, tx);
	}

	@Override
	public StashResult getItemStashable(HolderLookup.Provider registries, ItemStack storageStack, ItemStack stack) {
		IBackpackWrapper wrapper = BackpackWrapper.fromStack(storageStack);
		if (wrapper.getContentsUuid().isEmpty()) {
			return StashResult.SPACE; // Assuming that backpack that has no contentsUuid is empty and will have inventory once contentsUuid is created and thus
										// any item can be stashed into it
		}

		if (InventoryHelper.simulateInsert(wrapper.getInventoryForUpgradeProcessing(), ItemResource.of(stack), stack.getCount()) == 0) {
			return StashResult.NO_SPACE;
		}
		if (wrapper.getInventoryHandler().getSlotTracker().getItems().contains(stack.getItem())
				|| wrapper.getSettingsHandler().getTypeCategory(MemorySettingsCategory.class).matchesFilter(stack)) {
			return StashResult.MATCH_AND_SPACE;
		}

		return StashResult.SPACE;
	}

	public record BackpackContentsTooltip(ItemStack backpack) implements TooltipComponent {
		public ItemStack getBackpack() {
			return backpack;
		}
	}

	@Override
	public boolean overrideStackedOnOther(ItemStack storageStack, Slot slot, ClickAction action, Player player) {
		if (hasCreativeScreenContainerOpen(player) || storageStack.getCount() > 1 || !slot.mayPickup(player) || action != ClickAction.SECONDARY) {
			return super.overrideStackedOnOther(storageStack, slot, action, player);
		}

		ItemStack stackToStash = slot.getItem();
		int stashed;
		ItemResource resource = ItemResource.of(stackToStash);
		try (Transaction tx = Transaction.openRoot()) {
			stashed = stash(storageStack, resource, stackToStash.getCount(), tx);
		}
		if (stashed > 0) {
			slot.safeTake(stashed, stashed, player);
			try (Transaction tx = Transaction.openRoot()) {
				stash(storageStack, resource, stashed, tx);
				tx.commit();
			}
			return true;
		}

		return super.overrideStackedOnOther(storageStack, slot, action, player);
	}

	@Override
	public boolean overrideOtherStackedOnMe(ItemStack storageStack, ItemStack otherStack, Slot slot, ClickAction action, Player player,
			SlotAccess carriedAccess) {
		if (hasCreativeScreenContainerOpen(player) || storageStack.getCount() > 1 || !slot.mayPlace(storageStack) || action != ClickAction.SECONDARY) {
			return super.overrideOtherStackedOnMe(storageStack, otherStack, slot, action, player, carriedAccess);
		}

		try (Transaction tx = Transaction.openRoot()) {
			int stashed = stash(storageStack, ItemResource.of(otherStack), otherStack.getCount(), tx);
			if (stashed > 0) {
				carriedAccess.set(otherStack.copyWithCount(otherStack.getCount() - stashed));
				tx.commit();
				return true;
			}
		}

		return super.overrideOtherStackedOnMe(storageStack, otherStack, slot, action, player, carriedAccess);
	}

	private boolean hasCreativeScreenContainerOpen(Player player) {
		return player.level().isClientSide() && player.containerMenu instanceof CreativeModeInventoryScreen.ItemPickerMenu;
	}

	@Override
	public boolean canFitInsideContainerItems() {
		return Config.SERVER.canBePlacedInContainerItems.get();
	}
}
