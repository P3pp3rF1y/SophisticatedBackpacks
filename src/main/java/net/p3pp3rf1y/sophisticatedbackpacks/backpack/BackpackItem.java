package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.ChatFormatting;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.client.gui.screens.inventory.CreativeModeInventoryScreen;
import net.minecraft.client.renderer.BlockEntityWithoutLevelRenderer;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.sounds.SoundSource;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.InteractionResultHolder;
import net.minecraft.world.SimpleMenuProvider;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.SlotAccess;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.ClickAction;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.inventory.tooltip.TooltipComponent;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.item.context.BlockPlaceContext;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.SoundType;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.material.FluidState;
import net.minecraft.world.level.material.Fluids;
import net.minecraft.world.phys.Vec3;
import net.minecraft.world.phys.shapes.CollisionContext;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.client.extensions.common.IClientItemExtensions;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.capabilities.ForgeCapabilities;
import net.minecraftforge.common.capabilities.ICapabilityProvider;
import net.minecraftforge.common.util.LazyOptional;
import net.minecraftforge.common.util.NonNullLazy;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.fml.util.ObfuscationReflectionHelper;
import net.minecraftforge.fml.util.thread.SidedThreadGroups;
import net.minecraftforge.network.NetworkHooks;
import net.minecraftforge.server.ServerLifecycleHooks;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackLinkedStorageResolver;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.ClientLinkedStorageBackpackContents;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.LinkedStorageBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.LinkedStorageJukeboxPlaybackAnchors;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.BackpackItemStackRenderer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContext;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.network.RequestLinkedStorageBackpackContentsMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.network.SBPPacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.everlasting.EverlastingBackpackItemEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.everlasting.EverlastingUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryInteractionHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.api.IStashStorageItem;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.TranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointRole;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointStackState;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageService;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackLifecycle;
import net.p3pp3rf1y.sophisticatedcore.settings.memory.MemorySettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.ServerStorageSoundHandler;
import net.p3pp3rf1y.sophisticatedcore.util.ColorHelper;
import net.p3pp3rf1y.sophisticatedcore.util.ItemBase;
import net.p3pp3rf1y.sophisticatedcore.util.WorldHelper;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import java.util.List;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.IntSupplier;
import java.util.function.Supplier;
import java.util.function.UnaryOperator;

import static net.minecraft.world.level.block.state.properties.BlockStateProperties.WATERLOGGED;

public class BackpackItem extends ItemBase implements IStashStorageItem {
	public static final int DEFAULT_MAIN_COLOR = BackpackWrapper.DEFAULT_CLOTH_COLOR;
	public static final int DEFAULT_ACCENT_COLOR = BackpackWrapper.DEFAULT_BORDER_COLOR;

	private static final String CLOTH_COLOR_TAG = "clothColor";
	private static final String BORDER_COLOR_TAG = "borderColor";

	private final IntSupplier numberOfSlots;
	private final IntSupplier numberOfUpgradeSlots;
	private final Supplier<BackpackBlock> blockSupplier;

	public BackpackItem(IntSupplier numberOfSlots, IntSupplier numberOfUpgradeSlots, Supplier<BackpackBlock> blockSupplier) {
		this(numberOfSlots, numberOfUpgradeSlots, blockSupplier, p -> p);
	}

	public BackpackItem(IntSupplier numberOfSlots, IntSupplier numberOfUpgradeSlots, Supplier<BackpackBlock> blockSupplier,
			UnaryOperator<Properties> updateProperties) {
		super(updateProperties.apply(new Properties().stacksTo(1)));
		this.numberOfSlots = numberOfSlots;
		this.numberOfUpgradeSlots = numberOfUpgradeSlots;
		this.blockSupplier = blockSupplier;
	}

	public static void setColors(ItemStack backpackStack, int mainColor, int accentColor) {
		backpackStack.getOrCreateTag().putInt(CLOTH_COLOR_TAG, mainColor);
		backpackStack.getOrCreateTag().putInt(BORDER_COLOR_TAG, accentColor);
	}

	public static int getMainColor(ItemStack backpackStack) {
		CompoundTag tag = backpackStack.getTag();
		return tag != null && tag.contains(CLOTH_COLOR_TAG) ? tag.getInt(CLOTH_COLOR_TAG) : DEFAULT_MAIN_COLOR;
	}

	public static int getAccentColor(ItemStack backpackStack) {
		CompoundTag tag = backpackStack.getTag();
		return tag != null && tag.contains(BORDER_COLOR_TAG) ? tag.getInt(BORDER_COLOR_TAG) : DEFAULT_ACCENT_COLOR;
	}

	public static Optional<LinkedStorageEndpointRole> getLinkedStorageEndpointRole(ItemStack backpackStack) {
		if (LinkedStorageStackLifecycle.classifyEndpoint(backpackStack) != LinkedStorageEndpointStackState.ENDPOINT) {
			return Optional.empty();
		}
		return Optional.of(LinkedStorageStackData.isPrimaryEndpoint(backpackStack) ? LinkedStorageEndpointRole.PRIMARY : LinkedStorageEndpointRole.SECONDARY);
	}

	public static boolean shouldRenderUpgradeActivity(ItemStack backpackStack) {
		return getLinkedStorageEndpointRole(backpackStack).map(role -> role == LinkedStorageEndpointRole.PRIMARY).orElse(true);
	}

	@Override
	public void initializeClient(Consumer<IClientItemExtensions> consumer) {
		consumer.accept(new IClientItemExtensions() {
			private final NonNullLazy<BlockEntityWithoutLevelRenderer> ister = NonNullLazy.of(
					() -> new BackpackItemStackRenderer(Minecraft.getInstance().getBlockEntityRenderDispatcher(), Minecraft.getInstance().getEntityModels()));

			@Override
			public BlockEntityWithoutLevelRenderer getCustomRenderer() {
				return ister.get();
			}
		});
	}

	@Override
	public void addCreativeTabItems(Consumer<ItemStack> itemConsumer) {
		super.addCreativeTabItems(itemConsumer);

		if (this != ModItems.BACKPACK.get() || !net.p3pp3rf1y.sophisticatedcore.Config.COMMON.enabledItems.isItemEnabled(this)) {
			return;
		}

		for (DyeColor color : DyeColor.values()) {
			ItemStack stack = new ItemStack(this);
			new BackpackWrapper(stack).setColors(ColorHelper.getColor(color.getTextureDiffuseColors()), ColorHelper.getColor(color.getTextureDiffuseColors()));
			itemConsumer.accept(stack);
		}

		int clothColor = ColorHelper.calculateColor(BackpackWrapper.DEFAULT_CLOTH_COLOR, BackpackWrapper.DEFAULT_CLOTH_COLOR,
				List.of(DyeColor.BLUE, DyeColor.YELLOW, DyeColor.LIME));
		int trimColor = ColorHelper.calculateColor(BackpackWrapper.DEFAULT_BORDER_COLOR, BackpackWrapper.DEFAULT_BORDER_COLOR,
				List.of(DyeColor.BLUE, DyeColor.BLACK));

		ItemStack stack = new ItemStack(this);
		new BackpackWrapper(stack).setColors(clothColor, trimColor);
		itemConsumer.accept(stack);
	}

	@Override
	public void appendHoverText(ItemStack stack, @Nullable Level worldIn, List<Component> tooltip, TooltipFlag flagIn) {
		super.appendHoverText(stack, worldIn, tooltip, flagIn);
		LinkedStorageEndpointData endpoint = LinkedStorageStackData.getEndpoint(stack);
		if (worldIn != null && worldIn.isClientSide && endpoint != null) {
			long revision = ClientLinkedStorageBackpackContents.getRevision(endpoint.groupId()).orElse(-1L);
			SBPPacketHandler.INSTANCE.sendToServer(new RequestLinkedStorageBackpackContentsMessage(endpoint.groupId(), revision));
		}
		if (flagIn == TooltipFlag.ADVANCED) {
			LinkedStorageEndpointData advancedEndpoint = LinkedStorageStackData.getEndpoint(stack);
			if (advancedEndpoint != null) {
				tooltip.add(Component
						.translatable(TranslationHelper.INSTANCE.translItemTooltip("storage") + ".linked_storage_group", advancedEndpoint.groupId().toString())
						.withStyle(ChatFormatting.DARK_GRAY));
				tooltip.add(Component.translatable(TranslationHelper.INSTANCE.translItemTooltip("storage") + ".linked_storage_endpoint",
						advancedEndpoint.endpointId().toString()).withStyle(ChatFormatting.DARK_GRAY));
			} else {
				stack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(
						w -> w.getContentsUuid().ifPresent(uuid -> tooltip.add(Component.literal("UUID: " + uuid).withStyle(ChatFormatting.DARK_GRAY))));
			}
		}
		if (!Screen.hasShiftDown()) {
			tooltip.add(Component
					.translatable(TranslationHelper.INSTANCE.translItemTooltip("storage") + ".press_for_contents",
							Component.translatable(TranslationHelper.INSTANCE.translItemTooltip("storage") + ".shift").withStyle(ChatFormatting.AQUA))
					.withStyle(ChatFormatting.GRAY));
		}
	}

	@Override
	public Optional<TooltipComponent> getTooltipImage(ItemStack stack) {
		AtomicReference<TooltipComponent> ret = new AtomicReference<>(null);
		DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> () -> {
			Minecraft mc = Minecraft.getInstance();
			Optional<LinkedStorageEndpointRole> linkedStorageRole = getLinkedStorageEndpointRole(stack);
			if (linkedStorageRole.isPresent() && !Screen.hasShiftDown() && (mc.player == null || mc.player.containerMenu.getCarried().isEmpty())) {
				LinkedStorageEndpointData endpoint = LinkedStorageStackData.getEndpoint(stack);
				if (ClientLinkedStorageBackpackContents.getGroupName(endpoint.groupId()).isEmpty()
						&& ClientLinkedStorageBackpackContents.requestGroupName(endpoint.groupId())) {
					SBPPacketHandler.INSTANCE.sendToServer(new RequestLinkedStorageBackpackContentsMessage(endpoint.groupId(), -1L));
				}
				ret.set(new LinkedStorageTooltip(linkedStorageRole.get(), endpoint.groupId()));
				return;
			}
			if (Screen.hasShiftDown() || (mc.player != null && !mc.player.containerMenu.getCarried().isEmpty())) {
				ret.set(new BackpackContentsTooltip(stack));
			}
		});
		return Optional.ofNullable(ret.get());
	}

	@Override
	public boolean hasCustomEntity(ItemStack stack) {
		return true;
	}

	private boolean hasEverlastingUpgrade(Level level, ItemStack stack) {
		return !BackpackLinkedStorageResolver.resolveForGlobalUpgradeProcessing(level, stack).getUpgradeHandler().getTypeWrappers(EverlastingUpgradeItem.TYPE)
				.isEmpty();
	}

	@Nullable
	@Override
	public Entity createEntity(Level world, Entity entity, ItemStack itemstack) {
		if (!(entity instanceof ItemEntity itemEntity)) {
			return null;
		}

		UUIDDeduplicator.dedupeBackpackItemEntityInArea(itemEntity);

		return hasEverlastingUpgrade(world, itemstack) ? createEverlastingBackpack(world, (ItemEntity) entity, itemstack) : null;
	}

	@Nullable
	private EverlastingBackpackItemEntity createEverlastingBackpack(Level world, ItemEntity itemEntity, ItemStack itemstack) {
		EverlastingBackpackItemEntity backpackItemEntity = ModItems.EVERLASTING_BACKPACK_ITEM_ENTITY.get().create(world);
		if (backpackItemEntity != null) {
			backpackItemEntity.setPos(itemEntity.getX(), itemEntity.getY(), itemEntity.getZ());
			backpackItemEntity.setItem(itemstack);
			backpackItemEntity.setPickUpDelay(getPickupDelay(itemEntity));
			backpackItemEntity.setThrower(itemEntity.getOwner() != null ? itemEntity.getOwner().getUUID() : null);
			backpackItemEntity.setDeltaMovement(itemEntity.getDeltaMovement());
		}
		return backpackItemEntity;
	}

	private int getPickupDelay(ItemEntity itemEntity) {
		Integer result = ObfuscationReflectionHelper.getPrivateValue(ItemEntity.class, itemEntity, "f_31986_");
		if (result == null) {
			SophisticatedBackpacks.LOGGER.error("Reflection get of pickupDelay (pickupDelay) from ItemEntity returned null");
			return 20;
		}
		return result;
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
		Level world = blockItemUseContext.getLevel();
		BlockPos pos = blockItemUseContext.getClickedPos();
		ItemStack backpack = blockItemUseContext.getItemInHand();
		boolean creativeLinkedPlacement = player != null && player.isCreative() && world instanceof ServerLevel
				&& LinkedStorageStackLifecycle.classifyEndpoint(backpack) == LinkedStorageEndpointStackState.ENDPOINT;
		ItemStack placedBackpack = creativeLinkedPlacement ? ItemStack.EMPTY : getBackpackCopy(player, backpack);

		FluidState fluidstate = blockItemUseContext.getLevel().getFluidState(pos);
		BlockState placementState = blockSupplier.get().defaultBlockState().setValue(BackpackBlock.FACING, direction).setValue(WATERLOGGED,
				fluidstate.getType() == Fluids.WATER);
		if (!canPlace(blockItemUseContext, placementState)) {
			return InteractionResult.FAIL;
		}

		if (world.setBlockAndUpdate(pos, placementState)) {
			BackpackBlockEntity backpackBlockEntity = WorldHelper.getBlockEntity(world, pos, BackpackBlockEntity.class).orElseThrow();
			if (creativeLinkedPlacement) {
				placedBackpack = LinkedStorageService.createSecondaryEndpointCopy((ServerLevel) world, backpack).orElseThrow();
			}
			backpackBlockEntity.setBackpack(placedBackpack);
			backpackBlockEntity.refreshRenderState();
			backpackBlockEntity.tryToAddToController();

			if (!world.isClientSide) {
				stopBackpackSounds(placedBackpack, world, pos);
			}

			SoundType soundtype = placementState.getSoundType(world, pos, player);
			world.playSound(player, pos, soundtype.getPlaceSound(), SoundSource.BLOCKS, (soundtype.getVolume() + 1.0F) / 2.0F, soundtype.getPitch() * 0.8F);
			if (player == null || !player.isCreative()) {
				blockItemUseContext.getItemInHand().shrink(1);
			}

			return InteractionResult.SUCCESS;
		}
		return InteractionResult.PASS;
	}

	private static void stopBackpackSounds(ItemStack backpack, Level world, BlockPos pos) {
		LinkedStorageEndpointData endpoint = LinkedStorageStackData.getEndpoint(backpack);
		if (endpoint != null) {
			if (world instanceof ServerLevel serverLevel && LinkedStorageJukeboxPlaybackAnchors.isPrimaryEndpoint(serverLevel, backpack)) {
				ServerStorageSoundHandler.stopPlayingDisc(serverLevel, Vec3.atCenterOf(pos), endpoint.groupId());
			}
			return;
		}
		backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(wrapper -> wrapper.getContentsUuid()
				.ifPresent(uuid -> ServerStorageSoundHandler.stopPlayingDisc((ServerLevel) world, Vec3.atCenterOf(pos), uuid)));
	}

	private ItemStack getBackpackCopy(@Nullable Player player, ItemStack backpack) {
		if (player == null || !player.isCreative()) {
			return backpack.copy();
		}
		if (LinkedStorageStackLifecycle.classifyEndpoint(backpack) == LinkedStorageEndpointStackState.ENDPOINT) {
			ItemStack copy = backpack.copy();
			LinkedStorageStackLifecycle.clear(copy);
			return copy;
		}
		return backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).map(IBackpackWrapper::cloneBackpack)
				.orElse(new ItemStack(ModItems.BACKPACK.get()));
	}

	protected boolean canPlace(BlockPlaceContext context, BlockState state) {
		Player playerentity = context.getPlayer();
		CollisionContext iselectioncontext = playerentity == null ? CollisionContext.empty() : CollisionContext.of(playerentity);
		return (state.canSurvive(context.getLevel(), context.getClickedPos()))
				&& context.getLevel().isUnobstructed(state, context.getClickedPos(), iselectioncontext);
	}

	@Override
	public InteractionResultHolder<ItemStack> use(Level world, Player player, InteractionHand hand) {
		ItemStack stack = player.getItemInHand(hand);

		if (!world.isClientSide && player instanceof ServerPlayer serverPlayer) {
			String handlerName = hand == InteractionHand.MAIN_HAND ? PlayerInventoryProvider.MAIN_INVENTORY : PlayerInventoryProvider.OFFHAND_INVENTORY;
			int slot = hand == InteractionHand.MAIN_HAND ? player.getInventory().selected : 0;
			BackpackContext.Item context = new BackpackContext.Item(handlerName, slot);
			NetworkHooks.openScreen(serverPlayer, new SimpleMenuProvider((w, p, pl) -> new BackpackContainer(w, pl, context), context.getDisplayName(player)),
					buffer -> context.toBuffer(buffer, player));
		}
		return InteractionResultHolder.success(stack);
	}

	@Override
	public ICapabilityProvider initCapabilities(ItemStack stack, @Nullable CompoundTag nbt) {
		return new ICapabilityProvider() {
			private IStorageWrapper wrapper = null;
			private boolean wrapperForLinkedEndpoint = false;

			@Nonnull
			@Override
			public <T> LazyOptional<T> getCapability(Capability<T> cap, @Nullable Direction side) {
				if (stack.getCount() > 1) {
					return LazyOptional.empty();
				}

				initWrapper();
				if (cap == CapabilityBackpackWrapper.getCapabilityInstance()) {
					return LazyOptional.of(() -> wrapper).cast();
				} else if (cap == ForgeCapabilities.ITEM_HANDLER) {
					return LazyOptional.of(() -> wrapper.getInventoryForInputOutput()).cast();
				} else if (cap == ForgeCapabilities.FLUID_HANDLER_ITEM && Boolean.TRUE.equals(Config.SERVER.itemFluidHandlerEnabled.get())) {
					return wrapper.getFluidHandler().<LazyOptional<T>>map(handler -> LazyOptional.of(() -> handler).cast()).orElseGet(LazyOptional::empty);
				} else if (cap == ForgeCapabilities.ENERGY) {
					return wrapper.getEnergyStorage().<LazyOptional<T>>map(storage -> LazyOptional.of(() -> storage).cast()).orElseGet(LazyOptional::empty);
				}
				return LazyOptional.empty();
			}

			private void initWrapper() {
				boolean linkedEndpoint = LinkedStorageStackLifecycle.classifyEndpoint(stack) == LinkedStorageEndpointStackState.ENDPOINT;
				if (wrapper == null || wrapperForLinkedEndpoint != linkedEndpoint) {
					if (wrapper instanceof LinkedStorageBackpackWrapper linkedStorageBackpackWrapper) {
						linkedStorageBackpackWrapper.close();
					}
					wrapperForLinkedEndpoint = linkedEndpoint;
					if (Thread.currentThread().getThreadGroup() == SidedThreadGroups.SERVER && ServerLifecycleHooks.getCurrentServer() != null) {
						wrapper = BackpackLinkedStorageResolver.resolveOrCreate(ServerLifecycleHooks.getCurrentServer().overworld(), stack);
					} else {
						// Client-side click prediction must not mutate the integrated server's canonical linked host.
						wrapper = linkedEndpoint ? IBackpackWrapper.Noop.INSTANCE : new BackpackWrapper(stack);
					}
				}
			}
		};
	}

	@Override
	public void onArmorTick(ItemStack stack, Level level, Player player) {
		if (player.isSpectator() || player.isDeadOrDying() || Boolean.FALSE.equals(Config.SERVER.nerfsConfig.onlyWornBackpackTriggersUpgrades.get())) {
			return;
		}
		if (level instanceof ServerLevel serverLevel && LinkedStorageStackLifecycle.classifyEndpoint(stack) == LinkedStorageEndpointStackState.ENDPOINT) {
			BackpackLinkedStorageResolver.resolvePrimaryCanonicalHost(serverLevel, stack).ifPresent(backpackWrapper -> {
				if (player instanceof ServerPlayer serverPlayer) {
					LinkedStorageJukeboxPlaybackAnchors.refreshPlayerAnchor(serverPlayer, stack);
				}
				backpackWrapper.getUpgradeHandler().getWrappersThatImplement(ITickableUpgrade.class)
						.forEach(upgrade -> upgrade.tick(player, player.level(), player.blockPosition()));
			});
			return;
		}
		stack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.ifPresent(wrapper -> wrapper.getUpgradeHandler().getWrappersThatImplement(ITickableUpgrade.class).forEach(upgrade -> {
					if (level.isClientSide) {
						upgrade.clientTick(player, player.level(), player.blockPosition());
					} else {
						upgrade.tick(player, player.level(), player.blockPosition());
					}
				}));
		super.onArmorTick(stack, level, player);
	}

	@Override
	public void inventoryTick(ItemStack stack, Level level, Entity entityIn, int itemSlot, boolean isSelected) {
		if (!(entityIn instanceof Player player) || player.isSpectator() || player.isDeadOrDying()) {
			return;
		}
		if (level instanceof ServerLevel serverLevel && LinkedStorageStackLifecycle.classifyEndpoint(stack) == LinkedStorageEndpointStackState.ENDPOINT) {
			if (BackpackLinkedStorageResolver.synchronizeRenderProjection(serverLevel, stack)) {
				player.inventoryMenu.broadcastChanges();
			}
			BackpackLinkedStorageResolver.resolvePrimaryCanonicalHost(serverLevel, stack).ifPresent(backpackWrapper -> {
				if (player instanceof ServerPlayer serverPlayer) {
					LinkedStorageJukeboxPlaybackAnchors.refreshPlayerAnchor(serverPlayer, stack);
				}
				if (!Config.SERVER.nerfsConfig.onlyWornBackpackTriggersUpgrades.get() || itemSlot <= -1) {
					backpackWrapper.getUpgradeHandler().getWrappersThatImplement(ITickableUpgrade.class)
							.forEach(upgrade -> upgrade.tick(player, player.level(), player.blockPosition()));
				}
			});
			super.inventoryTick(stack, level, entityIn, itemSlot, isSelected);
			return;
		}
		if (Config.SERVER.nerfsConfig.onlyWornBackpackTriggersUpgrades.get() && itemSlot > -1) {
			return;
		}
		stack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.ifPresent(wrapper -> wrapper.getUpgradeHandler().getWrappersThatImplement(ITickableUpgrade.class).forEach(upgrade -> {
					if (level.isClientSide) {
						upgrade.clientTick(player, player.level(), player.blockPosition());
					} else {
						upgrade.tick(player, player.level(), player.blockPosition());
					}
				}));
		super.inventoryTick(stack, level, entityIn, itemSlot, isSelected);
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
		return Optional.of(new BackpackItem.BackpackContentsTooltip(stack));
	}

	public ItemStack stash(ItemStack storageStack, ItemStack stack, boolean simulate) {
		return storageStack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.map(wrapper -> wrapper.getInventoryForUpgradeProcessing().insertItem(stack, simulate)).orElse(stack);
	}

	@Override
	public StashResult getItemStashable(ItemStack storageStack, ItemStack stack) {
		return storageStack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).map(wrapper -> {
			if (wrapper.getContentsUuid().isEmpty()) {
				return StashResult.SPACE; // Assuming that backpack that has no contentsUuid is empty and will have inventory once contentsUuid is created and
											// thus any item can be stashed into it
			}

			if (wrapper.getInventoryForUpgradeProcessing().insertItem(stack, true).getCount() == stack.getCount()) {
				return StashResult.NO_SPACE;
			}
			if (wrapper.getInventoryHandler().getSlotTracker().getItems().contains(stack.getItem())
					|| wrapper.getSettingsHandler().getTypeCategory(MemorySettingsCategory.class).matchesFilter(stack)) {
				return StashResult.MATCH_AND_SPACE;
			}

			return StashResult.SPACE;
		}).orElse(StashResult.NO_SPACE);
	}

	public record BackpackContentsTooltip(ItemStack backpack) implements TooltipComponent {
		public ItemStack getBackpack() {
			return backpack;
		}
	}

	public record LinkedStorageTooltip(LinkedStorageEndpointRole role, @Nullable UUID groupId) implements TooltipComponent {
	}

	@Override
	public boolean overrideStackedOnOther(ItemStack storageStack, Slot slot, ClickAction action, Player player) {
		if (hasCreativeScreenContainerOpen(player) || storageStack.getCount() > 1 || !slot.mayPickup(player) || action != ClickAction.SECONDARY) {
			return super.overrideStackedOnOther(storageStack, slot, action, player);
		}

		ItemStack stackToStash = slot.getItem();
		ItemStack stashResult = stash(storageStack, stackToStash, true);
		if (stashResult.getCount() < stackToStash.getCount()) {
			int countToTake = stackToStash.getCount() - stashResult.getCount();
			while (countToTake > 0) {
				ItemStack takeResult = slot.safeTake(countToTake, countToTake, player);
				if (takeResult.isEmpty()) {
					break;
				}
				stash(storageStack, takeResult, false);
				countToTake -= takeResult.getCount();
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

		ItemStack result = stash(storageStack, otherStack, false);
		if (result.getCount() != otherStack.getCount()) {
			carriedAccess.set(result);
			slot.set(storageStack);
			return true;
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
