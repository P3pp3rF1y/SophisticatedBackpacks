package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.SharedConstants;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.server.Bootstrap;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.mockito.Mockito.*;

class InventoryInteractionHelperSecurityTest {
	@BeforeAll
	static void bootstrap() {
		SharedConstants.tryDetectVersion();
		Bootstrap.bootStrap();
	}

	@Test
	void tryInventoryInteractionRejectsUnloadedTargetBeforeReadingBlock() {
		Level level = mock(Level.class);
		Player player = mock(Player.class);
		BlockPos pos = new BlockPos(1, 64, 1);
		when(player.level()).thenReturn(level);

		boolean interacted = InventoryInteractionHelper.tryInventoryInteraction(pos, level, ItemStack.EMPTY, Direction.UP, player);

		assertFalse(interacted);
		verify(level, never()).getBlockState(any());
		verify(player, never()).canInteractWithBlock(any(), anyDouble());
	}

	@Test
	void tryInventoryInteractionRejectsDistantTargetBeforeReadingBlock() {
		Level level = mock(Level.class);
		Player player = mock(Player.class);
		BlockPos pos = new BlockPos(1, 64, 1);
		when(player.level()).thenReturn(level);
		when(level.hasChunkAt(pos)).thenReturn(true);
		when(player.canInteractWithBlock(pos, 0)).thenReturn(false);

		boolean interacted = InventoryInteractionHelper.tryInventoryInteraction(pos, level, ItemStack.EMPTY, Direction.UP, player);

		assertFalse(interacted);
		verify(level, never()).getBlockState(any());
		verify(player, never()).mayInteract(any(), any());
	}
}
